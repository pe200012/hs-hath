-- | The request handlers, one per route of "Hath.Server.API".
module Hath.Server.Handlers ( application ) where

import           Control.Concurrent       ( forkIO, threadDelay )

import           Data.ByteString.Builder  ( byteString )
import qualified Data.ByteString.Char8    as BS
import qualified Data.Map.Strict          as Map
import           Data.String.Interpolate  ( i )

import           Hath.Cache               ( CacheHit(..) )
import           Hath.Config              ( ClientConfig )
import           Hath.Download            ( fetchFile )
import           Hath.Error               ( HathError )
import           Hath.FileURI             ( FileURI(fileSize), mimeType, parseFileURI )
import           Hath.Locate              ( Located(..), locateResource )
import           Hath.Log                 ( logInfo, logWarning )
import           Hath.Protocol            ( commandTimeValid
                                          , fetchParams
                                          , keystampValid
                                          , parseURLParams
                                          , responseHeaders
                                          , serverCommandKey
                                          , speedTestKey
                                          )
import           Hath.RPC                 ( clientSettings )
import           Hath.Server.API          ( ProxyTest(..)
                                          , Route(..)
                                          , ServerCommand(..)
                                          , parseServerCommand
                                          , route
                                          )
import           Hath.Server.App          ( App, ServerAction(..), ServerEnv(..), runApp )
import           Hath.Server.FloodControl ( isRPCServer, sockAddrIP )
import           Hath.ServerTime          ( serverNow )
import           Hath.Settings            ( HathSettings(maxAllowedFileSize, rpcServerIPs) )
import           Hath.Stats               ( addDownload, addUpload, incServed, readMetrics )

import qualified Network.HTTP.Client      as HC
import           Network.HTTP.Types       ( Header
                                          , Status
                                          , hCacheControl
                                          , hContentLength
                                          , hLocation
                                          , methodGet
                                          , methodHead
                                          , status200
                                          , status301
                                          , status302
                                          , status400
                                          , status403
                                          , status404
                                          , statusCode
                                          )
import           Network.Socket           ( SockAddr )
import qualified Network.Wai              as Wai

import           Polysemy                 ( embed )
import           Polysemy.Error           ( catch, throw )
import           Polysemy.Reader          ( ask )

import           Relude                   hiding ( ask )

import           UnliftIO                 ( getMonotonicTime
                                          , replicateConcurrently
                                          , timeout
                                          , writeTChan
                                          )

-- | The client's HTTP interface. Like the official client, it reads GET and
-- HEAD requests only (HTTPSession.java:107), and answers a HEAD as the GET,
-- without the body: Warp leaves that out.
application :: ServerEnv -> Wai.Application
application env req respond
  | Wai.requestMethod req `notElem` [ methodGet, methodHead ] = respond (errorPage status400)
  | otherwise = runApp env (handle env req) >>= respond . either errorPage id

handle :: ServerEnv -> Wai.Request -> App Wai.Response
handle env req = case route (Wai.rawPathInfo req) of
  Favicon -> pure $ redirect status301 "https://e-hentai.org/favicon.ico" []
  Robots -> pure $ bytes status200 "text/plain; charset=ISO-8859-1" "User-agent: *\nDisallow: /"
  File fileId options -> resource env (Wai.requestMethod req == methodHead) fileId options
  ServerCmd command additional time commandKey
    -> serverCommand env (Wai.remoteHost req) command additional time commandKey
  Test size time testKey -> speedTest size time testKey
  Metrics -> ours <$> readMetrics
  AdminSettings -> ours . show <$> embed @IO (readTVarIO (envSettings env))
  Refuse status -> throw status
  where
    -- /metrics and /admin/settings are not the official client's; what they
    -- say changes, so they do not carry its year of Cache-Control
    ours
      = Wai.mapResponseHeaders (filter ((/= hCacheControl) . fst))
      . bytes status200 "text/plain;charset=utf-8"
      . encodeUtf8

-- | @\/h\/\<file id>\/\<options>@: a file for a reader (HTTPResponse.java:161-228).
-- The options carry the keystamp the H\@H server issued for this file, which
-- entitles the reader to it, and the @fileindex@ and @xres@ to fetch it by.
resource :: ServerEnv -> Bool -> ByteString -> ByteString -> App Wai.Response
resource env headOnly fileId optionsText = do
  now <- embed serverNow
  cfg <- ask @ClientConfig
  unless (keystampValid cfg now fileId (fromMaybe "" (Map.lookup "keystamp" options)))
    $ throw status403
  ( uri, fetchBy )
    <- maybe (throw status404) pure ((,) <$> parseFileURI fileId <*> fetchParams options)
  let headers         = responseHeaders (mimeType uri) (fileSize uri)
      served response = response <$ unless headOnly (incServed >> addUpload (fileSize uri))
  settings <- embed @IO (readTVarIO (envSettings env))
  locateResource (maxAllowedFileSize settings) fileId uri fetchBy >>= either throw pure >>= \case
    Stored (Redirect url)
      -> pure $ redirect status302 url [ ( "Cache-Control", "public, max-age=600" ) ]
    -- Warp sets the length of a file it sends, and answers Range and If-Modified-Since
    Stored (LocalFile path) -> served
      $ Wai.responseFile status200 (filter ((/= hContentLength) . fst) headers) path Nothing
    Stored (InMemory content) -> served $ Wai.responseLBS status200 headers (fromStrict content)
    -- a cache miss: relay upstream bytes as they arrive, like the official proxy
    -- mode. Warp does not run the body of a HEAD response; the file is fetched
    -- all the same, as by the official client.
    Streamed relay -> served $ Wai.responseStream status200 headers $ \write flush -> relay
      (\chunk -> write (byteString chunk) >> flush)
  where
    options = parseURLParams optionsText

-- | @\/servercmd\/\<command>\/\<additional>\/\<time>\/\<key>@: a command from the
-- RPC server (HTTPResponse.java:230-260). It must come from one of the RPC
-- servers' addresses, as flood control sees the peer, and its key proves the
-- sender: only the RPC server knows ours.
serverCommand :: ServerEnv -> SockAddr -> Text -> Text -> Int -> Text -> App Wai.Response
serverCommand env peer command additional time commandKey = do
  settings <- embed @IO (readTVarIO (envSettings env))
  now <- embed serverNow
  cfg <- ask @ClientConfig
  unless (maybe False (isRPCServer (rpcServerIPs settings)) (sockAddrIP peer)) $ throw status403
  unless
    (commandTimeValid now (fromIntegral time)
     && commandKey == serverCommandKey cfg command additional time)
    $ throw status403
  maybe
    (pure $ text "INVALID_COMMAND")
    (runCommand env)
    (parseServerCommand command (parseURLParams (encodeUtf8 additional)))

runCommand :: ServerEnv -> ServerCommand -> App Wai.Response
runCommand env = \case
  StillAlive -> pure $ text "I feel FANTASTIC and I'm still alive"
  ThreadedProxyTest test -> text <$> runProxyTest env test
  SpeedTest size -> do
    addDownload size
    pure $ filler size
  RefreshSettings -> do
    logInfo "Refreshing settings"
    -- the official client answers the same whether the refresh worked
    refresh `catch` \(e :: HathError) -> logWarning [i|Failed to refresh settings: #{e}|]
    pure $ text ""
  StartDownloader -> do
    logInfo "Starting gallery downloader"
    void $ embed @IO $ tryPutMVar (envDownloadRequest env) ()
    pure $ text ""
  RefreshCerts -> do
    logInfo "Refreshing certificates"
    void $ embed $ forkIO $ do
      threadDelay (2 * 1000000)
      atomically $ writeTChan (envActions env) Cert
    pure $ text ""
  where
    refresh = do
      refreshed <- clientSettings =<< embed @IO (readTVarIO (envSettings env))
      embed @IO $ atomically $ writeTVar (envSettings env) refreshed

-- | @\/t\/\<size>\/\<time>\/\<key>@: a speed test, for the RPC server or for
-- another client it sent ('ThreadedProxyTest') (HTTPResponse.java:261-290).
speedTest :: Int -> Int64 -> ByteString -> App Wai.Response
speedTest size time testKey = do
  now <- embed serverNow
  cfg <- ask @ClientConfig
  unless (commandTimeValid now time && testKey == speedTestKey cfg size time) $ throw status403
  addUpload size
  pure $ filler size

bytes :: Status -> ByteString -> ByteString -> Wai.Response
bytes status contentType content
  = Wai.responseLBS status (responseHeaders contentType (BS.length content)) (fromStrict content)

-- | A reply of the official client's HTTPResponseProcessorText.
text :: ByteString -> Wai.Response
text = bytes status200 textType

-- | The page the official client answers an error with (HTTPResponse.getHTTPResponseProcessor).
errorPage :: Status -> Wai.Response
errorPage status = bytes status textType [i|An error has occurred. (#{statusCode status})|]

redirect :: Status -> ByteString -> [ Header ] -> Wai.Response
redirect status url headers
  = Wai.responseLBS status (( hLocation, url ) : headers <> responseHeaders textType 0) ""

-- | What the official client's text replies say they are; its other replies
-- without a file type spell it in lower case ('filler').
textType :: ByteString
textType = "text/html; charset=ISO-8859-1"

-- | @size@ filler bytes, produced one TCP segment's worth at a time
-- (Settings.TCP_PACKET_SIZE), as HTTPResponseProcessorSpeedtest sends them.
filler :: Int -> Wai.Response
filler size
  = Wai.responseStream status200 (responseHeaders "text/html; charset=iso-8859-1" (max 0 size))
  $ \write _ -> let
      go n = when (n > 0) $ write (byteString (BS.take n segment)) >> go (n - BS.length segment)
    in 
      go size
  where
    segment = BS.replicate 1460 '6'

-- | Run the downloads concurrently and report, in the official format,
-- @OK:\<successful tests>-\<total milliseconds of the successful tests>@.
runProxyTest :: ServerEnv -> ProxyTest -> App ByteString
runProxyTest env test = case HC.parseRequest (proxyTestURL test) of
  Nothing      -> pure "INVALID_COMMAND"
  Just request -> do
    settings <- embed @IO (readTVarIO (envSettings env))
    results <- embed
      $ replicateConcurrently
        (proxyTestCount test)
        (timedDownload (maxAllowedFileSize settings) (proxyTestSize test) request)
    let succeeded = catMaybes results
    addUpload (proxyTestCount test * fromIntegral (proxyTestSize test))
    pure [i|OK:#{length succeeded}-#{sum succeeded}|]

-- | A cap on one test download, its attempts included. The official client
-- hands FileDownloader 60 s (HTTPResponse.java:105), which it never enforces;
-- without a cap a target that trickles could hold the RPC server's request for minutes.
testTimeoutMicros :: Int
testTimeoutMicros = 60 * 1000000

-- | The milliseconds a test download took, from its first byte to its end, as
-- FileDownloader times it (FileDownloader.java:130-315): the first byte is the
-- first any of its attempts received. It counts if a body of the announced
-- length arrived, at least the test size (HTTPResponse.java:112).
timedDownload :: Int -> Int64 -> HC.Request -> IO (Maybe Int64)
timedDownload maxSize testSize request = do
  firstByte <- newIORef Nothing
  fetched <- timeout testTimeoutMicros $ fetchFile maxSize request $ \_ _
    -> whenNothingM_ (readIORef firstByte) (writeIORef firstByte . Just =<< getMonotonicTime)
  end <- getMonotonicTime
  start <- readIORef firstByte
  pure $ case fetched of
    Just (Right size)
      | fromIntegral size >= testSize -> Just $ maybe 0 (\t -> truncate ((end - t) * 1000)) start
    _ -> Nothing
