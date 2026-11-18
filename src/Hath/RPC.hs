{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Our side of the RPC with the H\@H server (protocol 1.6.6): the effect, its
-- HTTP interpreter, and one function per request.
module Hath.RPC
  ( -- * Effect
    RPC(..)
  , runRPC
  , runRPCIO
  , worthRetrying
  , httpManager
  , connectMicros
    -- * Session
  , serverStat
  , clientLogin
  , clientStart
  , StartFailure(..)
  , startFailure
  , clientStop
  , stillAlive
  , toldToStop
  , overload
  , clientSettings
  , getCertificate
  , getBlacklist
    -- * Files of our static ranges
  , fileSources
    -- * Gallery downloader
  , nextGallery
  , galleryFileSource
  , reportDownloadFailures
  , failureReport
  ) where

import           Colog                        ( Message, richMessageAction )
import           Colog.Polysemy               ( Log, runLogAction )

import           Control.Exception            ( try )

import           Crypto.Store.PKCS12          ( readP12FileFromMemory
                                              , recover
                                              , recoverAuthenticated
                                              , toCredential
                                              , toProtectionPassword
                                              )

import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.String.Interpolate      ( i )
import qualified Data.Text                    as T
import           Data.X509                    ( CertificateChain, PrivKey )

import           Hath.Config                  ( ClientConfig(..) )
import           Hath.Error                   ( HathError(..) )
import           Hath.FileURI                 ( FileURI, parseFileURI )
import           Hath.Gallery                 ( GalleryFile(..)
                                              , GalleryMetadata(..)
                                              , parseMetadata
                                              )
import           Hath.Log                     ( logInfo, logWarning )
import           Hath.Protocol                ( clientBuild
                                              , outgoingHeaders
                                              , retryWithServerTime
                                              , rpcPayload
                                              , rpcQuery
                                              , verifyRPCResponse
                                              )
import           Hath.ServerTime              ( serverNow, setServerTime )
import           Hath.Settings                ( HathSettings
                                              , defaultHathSettings
                                              , parseSettings
                                              , settingNumber
                                              )

import qualified Network.HTTP.Client          as HC
import           Network.HTTP.Client.Internal ( throwHttp )
import           Network.HTTP.Client.TLS      ( tlsManagerSettings )
import           Network.HTTP.Types           ( statusCode, statusIsSuccessful )

import           Polysemy
import           Polysemy.Error               ( Error, errorToIOFinal, fromEither, throw )
import           Polysemy.Reader              ( Reader, ask, runReader )

import           Relude                       hiding ( Reader, ask, runReader )

import           System.IO.Unsafe             ( unsafePerformIO )
import           System.Timeout               ( timeout )

-- | A request to the RPC server: its @act@ and @add@ (empty when it has no arguments).
data RPC m a where
  -- | @GET \/16\/rpc@. The reply's signature has been checked: see 'verifyRPCResponse'.
  CallRPC :: Text -> Text -> RPC m ByteString
  -- | @GET \/16\/dl@, the gallery downloader's endpoint, whose replies carry their
  -- own signature ('parseMetadata' checks it).
  CallDownloader :: Text -> Text -> RPC m ByteString

makeSem ''RPC

runRPC
  :: Members '[ Embed IO, Error HathError, Reader ClientConfig ] r => Sem (RPC : r) a -> Sem r a
runRPC = interpret $ \case
  -- get_cert answers with the PKCS#12 file itself, which is not a signed reply
  CallRPC "get_cert" add -> request "/16/rpc" "get_cert" add
  CallRPC act add        -> do
    reply <- signed act add
    if retryWithServerTime add reply
      then do
        stat <- signed "server_stat" ""
        whenRight_ (rpcPayload stat) (embed . recordServerTime)
        signed act add
      else pure reply
  CallDownloader act add -> request "/16/dl" act add
  where
    signed act add = do
      cfg <- ask @ClientConfig
      verifyRPCResponse (encodeUtf8 (key cfg)) <$> request "/16/rpc" act add

-- | Run RPC requests on their own, outside the web server.
runRPCIO :: ClientConfig
         -> Sem '[ RPC, Log Message, Reader ClientConfig, Error HathError, Embed IO, Final IO ] a
         -> IO (Either HathError a)
runRPCIO cfg
  = runFinal
  . embedToFinal @IO
  . errorToIOFinal @HathError
  . runReader cfg
  . runLogAction @IO richMessageAction
  . runRPC

-- | The RPC server, unless @HATH_RPC_HOST@ (@host@ or @host:port@) names another;
-- the tests and hath-bench point us at theirs that way.
rpcServer :: IO ( String, Int )
rpcServer = lookupEnv "HATH_RPC_HOST" <&> \case
  Nothing       -> ( "rpc.hentaiathome.net", 80 )
  Just override -> case break (== ':') override of
    ( host, ':' : port ) -> ( host, fromMaybe 80 (readMaybe port) )
    ( host, _ )          -> ( host, 80 )

-- | How long to wait for a reply, connecting included. The official client
-- waits up to an hour; a reader waiting on @srfetch@ would not.
rpcTimeoutMicros :: Int
rpcTimeoutMicros = 60 * 1000000

-- | The connections of every request we send: to the RPC server, for proxy
-- tests, to image servers. Each gives up after 'connectMicros' if it cannot
-- connect, as the official client's do (FileDownloader.java:165,
-- ProxyFileDownloader.java:86). http-client has no setting for that alone (its
-- response timeout counts the connection too), so every request sets its own
-- response timeout.
httpManager :: HC.Manager
httpManager
  = unsafePerformIO
  $ HC.newManager
    tlsManagerSettings
    { HC.managerRawConnection = within5s <$> HC.managerRawConnection tlsManagerSettings
    , HC.managerTlsConnection = within5s <$> HC.managerTlsConnection tlsManagerSettings
    }
  where
    within5s connect address host port
      = timeout connectMicros (connect address host port)
      >>= maybe (throwHttp HC.ConnectionTimeout) pure

{-# NOINLINE httpManager #-}

-- | How long a connection of 'httpManager' may take to open.
connectMicros :: Int
connectMicros = 5 * 1000000

-- | One GET to the RPC server, signed with the client key and the server's time.
request :: Members '[ Embed IO, Error HathError, Reader ClientConfig ] r
        => ByteString
        -> Text
        -> Text
        -> Sem r ByteString
request endpoint act add = do
  cfg <- ask @ClientConfig
  reply <- embed $ do
    ( host, port ) <- rpcServer
    time <- serverNow
    exchange
      HC.defaultRequest
      { HC.host = BS.pack host
      , HC.port = port
      , HC.path = endpoint
      , HC.queryString = rpcQuery cfg act add time
      , HC.requestHeaders = outgoingHeaders [ ( "Connection", "Close" ) ]
      , HC.responseTimeout = HC.responseTimeoutMicro rpcTimeoutMicros
      }
  fromEither reply

-- | The longest reply we take, as the official client reads one into memory
-- (FileDownloader.java:186). The RPC server is reached over plain HTTP: who
-- sits in between could otherwise send a reply without end.
maxReplyBytes :: Int
maxReplyBytes = 10485760

-- | Send a request and read the body of its 2xx reply. Like the official
-- FileDownloader, make up to three attempts, one right after another
-- (FileDownloader.java:147, 280-292).
exchange :: HC.Request -> IO (Either HathError ByteString)
exchange req = go (3 :: Int)
  where
    go attemptsLeft = attempt >>= \case
      Left ( status, _ )
        | attemptsLeft > 1, worthRetrying status -> go (attemptsLeft - 1)
      outcome -> pure $ first (TransportError . snd) outcome

    -- the reply's body, or else the status it came with (none if the exchange broke off) and why
    attempt          = try @HC.HttpException (HC.withResponse req httpManager reply) <&> \case
      Left (HC.HttpExceptionRequest _ content) -> Left ( Nothing, show content )
      Left e -> Left ( Nothing, show e )
      Right ( status, body )
        | not (statusIsSuccessful status) -> Left $ httpError $ statusCode status
        | tooLong body -> Left ( Nothing, "a reply of more than 10 MiB" )
        | otherwise -> Right $ toStrict body

    -- one byte past the limit shows that there was more
    reply response
      = (,) (HC.responseStatus response)
      <$> HC.brReadSome (HC.responseBody response) (maxReplyBytes + 1)

    tooLong body = LBS.length body > fromIntegral maxReplyBytes

    httpError status = ( Just status, "HTTP " <> show status )

-- | Whether an attempt that failed with this HTTP status (none if the exchange
-- broke off) is worth repeating. FileDownloader repeats every failure but 404
-- and 410, which HttpURLConnection reports as FileNotFoundException.
worthRetrying :: Maybe Int -> Bool
worthRetrying = maybe True (`notElem` [ 404, 410 ])

payload :: Member (Error HathError) r => ByteString -> Sem r [ ByteString ]
payload = fromEither . rpcPayload

-- | The payload of an OK reply that carries settings. Like the official client,
-- take the server's clock from any of them that has it.
settingsPayload :: Members '[ RPC, Error HathError, Embed IO ] r => Text -> Sem r [ ByteString ]
settingsPayload act = do
  settings <- payload =<< callRPC act ""
  embed $ recordServerTime settings
  pure settings

recordServerTime :: [ ByteString ] -> IO ()
recordServerTime = traverse_ setServerTime . settingNumber "server_time"

-- | Check in with the RPC server and take its clock. Exit if it no longer
-- accepts our build (official Settings.java:187-196).
serverStat :: Members '[ RPC, Error HathError, Log Message, Embed IO ] r => Sem r ()
serverStat = do
  stat <- settingsPayload "server_stat"
  when (settingNumber "min_client_build" stat > Just clientBuild)
    $ embed @IO
    $ die
      [i|The H@H network no longer accepts client build #{clientBuild} (official 1.6.6), the one hs-hath implements. Update hs-hath.|]
  when (settingNumber "cur_client_build" stat > Just clientBuild)
    $ logWarning
      [i|The H@H network has a client newer than build #{clientBuild}, the one hs-hath implements.|]

clientLogin :: Members '[ RPC, Error HathError, Log Message, Embed IO ] r => Sem r HathSettings
clientLogin = do
  logInfo "Logging in"
  parseSettings defaultHathSettings <$> settingsPayload "client_login"

-- | Tell the server we are listening. It tests our port before it answers.
clientStart :: Members '[ RPC, Error HathError ] r => Sem r ()
clientStart = void $ payload =<< callRPC "client_start" ""

-- | What a failed client_start leaves us to do (official ServerHandler.notifyStart).
data StartFailure
  = Refused !Text
    -- ^ the server will not have us: exit, saying why
  | Unannounced !Text
    -- ^ say what went wrong and stay up without the network, as the official
    -- client does, so that the owner can test the port
  deriving ( Eq, Show )

-- | Read a failed client_start for a client listening on this port.
startFailure :: Int -> HathError -> StartFailure
startFailure port = \case
  RequestFailure status
    | "FAIL_OTHER_CLIENT_CONNECTED" `T.isPrefixOf` status -> Refused
      "The server detected that another client was already connected from this computer or local network. You can only have one client running per public IPv4 address."
    | "FAIL_CID_IN_USE" `T.isPrefixOf` status -> Refused
      "The server detected that another client is already using this client ID. If the client was not shut down properly and your IP changed, you may have to wait up to 24 hours for the old session to time out."
    | "FAIL_CONNECT_TEST" `T.isPrefixOf` status -> Unannounced
      [i|The client has failed the external connectivity test: the server could not connect to it. Check that port #{port} is open and forwarded to this computer, and that your incoming and outgoing IP address are the same. The client keeps running so that you can test the port; restart it once that is fixed.|]
  failure -> Unannounced
    [i|Startup notification failed (#{failure}). The client keeps running, but the network will not send it anything; restart it.|]

clientStop :: Members '[ RPC, Error HathError ] r => Sem r ()
clientStop = void $ payload =<< callRPC "client_stop" ""

stillAlive :: Members '[ RPC, Error HathError ] r => Sem r ()
stillAlive = void $ payload =<< callRPC "still_alive" ""

-- | Whether a failed still_alive means we must shut down: the server answers
-- @TERM_BAD_NETWORK@ to a client it cannot reach (official CakeSphere.run). Any
-- other failure waits for the next heartbeat.
toldToStop :: HathError -> Bool
toldToStop = \case
  RequestFailure status -> "TERM_BAD_NETWORK" `T.isPrefixOf` status
  _ -> False

-- | Tell the server we are close to our connection limit; it then sends us
-- fewer readers for a while (official ServerHandler.notifyOverload).
overload :: Members '[ RPC, Error HathError ] r => Sem r ()
overload = void $ payload =<< callRPC "overload" ""

-- | The settings as the server has them now, applied onto the ones we hold.
clientSettings
  :: Members '[ RPC, Error HathError, Embed IO ] r => HathSettings -> Sem r HathSettings
clientSettings current = parseSettings current <$> settingsPayload "client_settings"

-- | Our TLS certificate. The server sends a PKCS#12 file whose password is the client key.
getCertificate :: Members '[ RPC, Error HathError, Reader ClientConfig ] r
               => Sem r ( CertificateChain, PrivKey )
getCertificate = do
  p12 <- callRPC "get_cert" ""
  cfg <- ask @ClientConfig
  let password = encodeUtf8 (key cfg)
      opened   = do
        container <- readP12FileFromMemory p12
        ( _, pkcs12 ) <- recoverAuthenticated password container
        recover (toProtectionPassword password) (toCredential pkcs12)
  case opened of
    Left err -> throw $ CertificateFailure $ show err
    Right Nothing -> throw $ CertificateFailure "no credential"
    Right (Just credential) -> pure credential

-- | Files added to the blacklist in the last so many seconds. A line that is
-- not a file id is skipped, as the official client does.
getBlacklist :: Members '[ RPC, Error HathError ] r => Int -> Sem r [ FileURI ]
getBlacklist seconds
  = mapMaybe parseFileURI <$> (payload =<< callRPC "get_blacklist" (show seconds))

-- | URLs a file of our static ranges can be fetched from (@srfetch@), given
-- the @fileindex@ and @xres@ of the request that asked for it and the file id
-- as it asked for it.
fileSources :: Members '[ RPC, Error HathError ] r
            => ByteString
            -> ByteString
            -> ByteString
            -> Sem r [ ByteString ]
fileSources fileIndex xres fileId
  = payload =<< callRPC "srfetch" (decodeUtf8 $ BS.intercalate ";" [ fileIndex, xres, fileId ])

-- | The next gallery queued for download, or why there is none
-- (@NO_PENDING_DOWNLOADS@, @INVALID_REQUEST@, or a list that was refused). The
-- same request marks the gallery before it done and takes it off the queue
-- (GalleryDownloader.java:163-170).
nextGallery :: Members '[ RPC, Reader ClientConfig ] r
            => Maybe GalleryMetadata
            -> Sem r (Either Text GalleryMetadata)
nextGallery done = do
  cfg <- ask @ClientConfig
  parseMetadata (encodeUtf8 (key cfg)) <$> callDownloader "fetchqueue" (maybe "" marked done)
  where
    marked gallery = [i|#{galleryID gallery};#{galleryMinXRes gallery}|]

-- | Where to download a gallery file from (@dlfetch@), the first line of an OK
-- reply, as the official client takes it (ServerHandler.java:341-359); the last
-- argument numbers this attempt at the file, from 1.
galleryFileSource :: Members '[ RPC, Error HathError ] r
                  => GalleryMetadata
                  -> GalleryFile
                  -> Int
                  -> Sem r (Maybe ByteString)
galleryFileSource gallery file attempt
  = listToMaybe
  <$> (payload
       =<< callRPC
         "dlfetch"
         [i|#{galleryID gallery};#{galleryFilePage file};#{galleryFileIndex file};#{galleryFileXRes file};#{attempt}|])

reportDownloadFailures :: Member RPC r => [ Text ] -> Sem r ()
reportDownloadFailures = traverse_ (void . callRPC "dlfails") . failureReport

-- | The @add@ of a dlfails report: the distinct failures, as
-- @host-fileindex-xres@, if there are 1 to 50 of them. More than that probably
-- means the trouble is ours, so the official client keeps quiet
-- (ServerHandler.java:366-371).
failureReport :: [ Text ] -> Maybe Text
failureReport failures
  | count >= 1 && count <= 50 = Just (T.intercalate ";" distinct)
  | otherwise = Nothing
  where
    distinct = ordNub failures

    count    = length distinct
