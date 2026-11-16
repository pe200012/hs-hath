{-# LANGUAGE DataKinds #-}

-- | The request handlers, one per route of the 'API'.
module Hath.Server.Handlers ( server ) where

import           Control.Concurrent      ( forkIO, threadDelay )
import           Control.Exception       ( try )

import qualified Data.ByteString         as BS
import           Data.ByteString.Builder ( byteString )
import qualified Data.ByteString.Char8   as BSC
import qualified Data.Map.Strict         as Map
import           Data.String.Interpolate ( i )
import           Data.Time.Clock.System  ( SystemTime(systemSeconds), getSystemTime )

import           Hath.Cache              ( CacheHit(..) )
import           Hath.Config             ( ClientConfig )
import           Hath.FileURI            ( FileURI(fileSize), mimeType, parseFileURI )
import           Hath.Gallery.Downloader ( GalleryTask(..) )
import           Hath.Locate             ( Located(..), locateResource )
import           Hath.Log                ( logInfo )
import           Hath.Protocol           ( hathHeaders
                                         , keystampHash
                                         , maxCommandDrift
                                         , maxKeystampDrift
                                         , parseKeystamp
                                         , parseURLParams
                                         , serverCommandKey
                                         , speedTestKey
                                         )
import           Hath.RPC                ( clientSettings )
import           Hath.Server.API         ( API, Body, ServerCommand(..), renderServerCommand )
import           Hath.Server.App         ( App, ServerAction(..), ServerEnv(..) )
import           Hath.Settings           ( HathSettings )
import           Hath.Stats              ( addDownload, addUpload, incServed, readMetrics )

import qualified Network.HTTP.Client     as HC
import           Network.HTTP.Client.TLS ( tlsManagerSettings )
import qualified Network.HTTP.Types      as HT
import           Network.HTTP.Types      ( hContentLength, hContentType, status200, status404 )
import qualified Network.Wai             as Wai

import           Polysemy                ( embed )
import           Polysemy.Error          ( throw )
import           Polysemy.Reader         ( ask )

import           Relude                  hiding ( ask )

import           Servant                 ( (:<|>)(..)
                                         , Application
                                         , NoContent
                                         , ServerError(errBody, errHeaders)
                                         , ServerT
                                         , Tagged(..)
                                         , addHeader
                                         , err301
                                         , err302
                                         , err403
                                         , err404
                                         )
import qualified Servant.Types.SourceT   as Source

import           UnliftIO                ( getMonotonicTime
                                         , replicateConcurrently
                                         , timeout
                                         , writeTChan
                                         )

server :: ServerEnv -> ServerT API App
server env
  = favicon
  :<|> robots
  :<|> resource
  :<|> readMetrics
  :<|> serverCommand env
  :<|> speedTest
  :<|> adminSettings (envSettings env)
  :<|> Tagged fallback

favicon :: App NoContent
favicon = throw err301 { errHeaders = [ ( "Location", "https://e-hentai.org/favicon.ico" ) ] }

robots :: App Text
robots = pure "User-agent: *\nDisallow: /"

adminSettings :: TVar HathSettings -> App Text
adminSettings settings = show <$> embed @IO (readTVarIO settings)

-- | Any other path: HEAD is answered, nothing is found.
fallback :: Application
fallback req respond = respond $ case Wai.requestMethod req of
  "HEAD" -> Wai.responseLBS status200 hathHeaders ""
  _      -> Wai.responseLBS status404 [] ""

-- | @\/h\/\<file id>\/\<options>\/\<file name>@: a file for a reader. The options
-- carry the keystamp the H\@H server issued for this file, which is what
-- entitles the reader to it.
resource :: Text
         -> Text
         -> Text
         -> Wai.Request
         -> (Wai.Response -> IO Wai.ResponseReceived)
         -> App Wai.ResponseReceived
resource fileIdText optionsText _fileName req respond = case Wai.requestMethod req of
  -- answer HEAD like any other path instead of opening an upstream body nobody reads
  "HEAD" -> embed $ respond $ Wai.responseLBS status200 hathHeaders ""
  _      -> do
    now <- embed $ systemSeconds <$> getSystemTime
    cfg <- ask @ClientConfig
    when (abs (stampTime - now) > maxKeystampDrift)
      $ throw err403 { errBody = "Your time is out of sync. Please update your system time." }
    when (stampHash /= keystampHash cfg stampTime fileId)
      $ throw err403 { errBody = "Invalid key." }
    uri <- maybe (throw err404) pure (parseFileURI fileId)
    locateResource uri options >>= \case
      Nothing -> throw err404
      Just (Stored (Redirect url)) -> throw
        err302 { errHeaders = [ ( "Location", url ), ( "Cache-Control", "public, max-age=600" ) ] }
      Just (Stored (LocalFile path)) -> do
        incServed
        addUpload (fileSize uri)
        -- Warp streams the file and sets Content-Length from it
        embed
          $ respond
          $ Wai.responseFile status200 [ ( hContentType, mimeType uri ) ] path Nothing
      Just (Stored (InMemory bytes)) -> do
        incServed
        addUpload (BS.length bytes)
        embed
          $ respond
          $ Wai.responseLBS status200 (fileHeaders uri (BS.length bytes)) (fromStrict bytes)
      -- cache miss: relay upstream bytes as they arrive, like the official proxy mode
      Just (Streamed relay) -> do
        incServed
        addUpload (fileSize uri)
        embed
          $ respond
          $ Wai.responseStream status200 (fileHeaders uri (fileSize uri))
          $ \write flush -> relay (\chunk -> write (byteString chunk) >> flush)
  where
    fileId = encodeUtf8 fileIdText

    options = parseURLParams (encodeUtf8 optionsText)

    ( stampTime, stampHash ) = parseKeystamp (fromMaybe "" (Map.lookup "keystamp" options))

fileHeaders :: FileURI -> Int -> [ HT.Header ]
fileHeaders uri len = [ ( hContentType, mimeType uri ), ( hContentLength, show len ) ]

-- | @\/servercmd\/\<command>\/\<additional>\/\<time>\/\<key>@: a command from the
-- RPC server. As in the official client, the key proves the sender: only the
-- RPC server knows ours.
serverCommand :: ServerEnv -> ServerCommand -> Text -> Int -> Text -> App Body
serverCommand env command additional time commandKey = do
  now <- embed $ systemSeconds <$> getSystemTime
  cfg <- ask @ClientConfig
  when
    (abs (fromIntegral time - now) > maxCommandDrift
     || commandKey /= serverCommandKey cfg (renderServerCommand command) additional time)
    $ throw err403
  case command of
    StillAlive        -> body "I feel FANTASTIC and I'm still alive"
    ThreadedProxyTest -> maybe (throw err403) runProxyTest (parseProxyTest params)
    SpeedTest         -> do
      let size
            = maybe 1000000 (fromIntegral . fst) (BSC.readInteger =<< Map.lookup "testsize" params)
      addDownload size
      pure $ filler size
    RefreshSettings   -> do
      logInfo "Refreshing settings"
      refreshed <- clientSettings =<< embed @IO (readTVarIO (envSettings env))
      embed @IO $ atomically $ writeTVar (envSettings env) refreshed
      body ""
    StartDownloader   -> do
      logInfo "Starting gallery downloader"
      embed @IO $ atomically $ writeTChan (envGalleryTasks env) GalleryTask
      body ""
    RefreshCerts      -> do
      logInfo "Refreshing certificates"
      void $ embed $ forkIO $ do
        threadDelay (2 * 1000000)
        atomically $ writeTChan (envActions env) Cert
      body ""
  where
    params = parseURLParams (encodeUtf8 additional)

-- | @\/t\/\<size>\/\<time>\/\<key>\/\<anything>@: a speed test, for the RPC server
-- or for another client it sent ('ThreadedProxyTest').
speedTest :: Int -> Int64 -> Text -> Text -> App Body
speedTest size time testKey _ = do
  now <- embed $ systemSeconds <$> getSystemTime
  cfg <- ask @ClientConfig
  when (abs (time - now) > maxKeystampDrift) $ throw err403
  when (encodeUtf8 testKey /= speedTestKey cfg size time) $ throw err403
  addUpload size
  pure $ filler size

-- | A body sent in one piece.
body :: ByteString -> App Body
body bytes = pure $ addHeader @"Content-Length" (BS.length bytes) $ Source.source [ bytes ]

-- | A body of that many filler bytes, produced one TCP segment's worth at a time.
filler :: Int -> Body
filler size = addHeader @"Content-Length" size $ Source.fromStepT $ segments size
  where
    segments 0 = Source.Stop
    segments n
      | n >= segmentSize = Source.Yield segment (segments (n - segmentSize))
      | otherwise = Source.Yield (BS.take n segment) Source.Stop

    segmentSize = 1460

    segment     = BS.replicate segmentSize 54

-- | A download test against another client's @\/t\/@ route.
data ProxyTest
  = ProxyTest { proxyTestCount :: !Int, proxyTestSize :: !Int64, proxyTestURL :: !String }

parseProxyTest :: Map ByteString ByteString -> Maybe ProxyTest
parseProxyTest params = do
  hostname <- param "hostname"
  port <- param "port"
  size <- number =<< param "testsize"
  count <- number =<< param "testcount"
  time <- number @Int64 =<< param "testtime"
  testKey <- param "testkey"
  let protocol = fromMaybe "http" (param "protocol")
  pure
    ProxyTest
    { proxyTestCount = count
    , proxyTestSize  = size
    , proxyTestURL   = BSC.unpack
        $ mconcat
          [ protocol
          , "://"
          , hostname
          , ":"
          , port
          , "/t/"
          , show size
          , "/"
          , show time
          , "/"
          , testKey
          , "/0"
          ]
    }
  where
    param name = Map.lookup name params

    number :: Num a => ByteString -> Maybe a
    number = fmap (fromIntegral . fst) . BSC.readInteger

-- | Run the downloads concurrently and report, in the official format,
-- @OK:\<successful tests>-\<total milliseconds of the successful tests>@.
runProxyTest :: ProxyTest -> App Body
runProxyTest test = case HC.parseRequest (proxyTestURL test) of
  Nothing      -> throw err403
  Just request -> do
    results <- embed $ do
      manager <- HC.newManager tlsManagerSettings
      replicateConcurrently
        (proxyTestCount test)
        (timedDownload (proxyTestSize test) request manager)
    let succeeded = catMaybes results
    addUpload (proxyTestCount test * fromIntegral (proxyTestSize test))
    body [i|OK:#{length succeeded}-#{sum succeeded}|]

-- | Whole-download cap for one test.
-- ponytail: one total timeout; the official client uses 10 s connect + 60 s per read.
testTimeoutMicros :: Int
testTimeoutMicros = 60 * 1000000

-- | The milliseconds a download took, if the whole body arrived with status 200
-- within the timeout (official: FileDownloader + getContentLength() >= testsize).
timedDownload :: Int64 -> HC.Request -> HC.Manager -> IO (Maybe Int64)
timedDownload size request manager = do
  start <- getMonotonicTime
  outcome
    <- timeout testTimeoutMicros $ try @SomeException $ HC.withResponse request manager $ \response
    -> let
        drain n
          | n <= 0 = pure True
          | otherwise = do
            chunk <- HC.brRead (HC.responseBody response)
            -- an empty chunk is EOF: the body was shorter than promised
            if BS.null chunk
              then pure False
              else drain (n - fromIntegral (BS.length chunk))
      in 
        if HT.statusCode (HC.responseStatus response) == 200
          then drain size
          else pure False
  end <- getMonotonicTime
  pure $ case outcome of
    Just (Right True) -> Just (truncate ((end - start) * 1000))
    _ -> Nothing
