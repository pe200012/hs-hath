{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Our side of the RPC with the H\@H server (protocol 1.6.6): the effect, its
-- HTTP interpreter, and one function per request.
module Hath.RPC
  ( -- * Effect
    RPC(..)
  , runRPC
  , runRPCIO
    -- * Session
  , serverStat
  , clientLogin
  , clientStart
  , clientStop
  , stillAlive
  , clientSettings
  , getCertificate
  , getBlacklist
    -- * Files of our static ranges
  , fileSources
    -- * Gallery downloader
  , nextGallery
  , galleryFileSources
  , reportDownloadFailures
  , completeGallery
  ) where

import           Colog                   ( Message, richMessageAction )
import           Colog.Polysemy          ( Log, runLogAction )

import           Crypto.Store.PKCS12     ( readP12FileFromMemory
                                         , recover
                                         , recoverAuthenticated
                                         , toCredential
                                         , toProtectionPassword
                                         )

import qualified Data.ByteString.Char8   as BS
import           Data.String.Interpolate ( i )
import qualified Data.Text               as T
import           Data.Time.Clock.System  ( SystemTime(systemSeconds), getSystemTime )
import           Data.X509               ( CertificateChain, PrivKey )

import           Hath.Config             ( ClientConfig(..) )
import           Hath.Error              ( HathError(..) )
import           Hath.FileURI            ( FileURI, renderFileURIBytes )
import           Hath.Gallery            ( GalleryFile(..), GalleryMetadata(..), parseMetadata )
import           Hath.Log                ( logInfo )
import           Hath.Protocol           ( clientBuild
                                         , hathHeaders
                                         , rpcActionKey
                                         , rpcPayload
                                         , verifyRPCResponse
                                         )
import           Hath.Settings           ( HathSettings, defaultHathSettings, parseSettings )

import           Network.HTTP.Client     ( Request(responseTimeout), responseTimeoutMicro )
import           Network.HTTP.Client.TLS ( getGlobalManager )
import           Network.HTTP.Media      ( (//), (/:) )

import           Polysemy
import           Polysemy.Error          ( Error, errorToIOFinal, fromEither, throw )
import           Polysemy.Reader         ( Reader, ask, runReader )

import           Relude                  hiding ( Reader, ask, runReader )

import           Servant.API             ( (:>), Accept(..), Get, MimeUnrender(..), OctetStream, QueryParam )
import           Servant.Client          ( BaseUrl(BaseUrl)
                                         , ClientEnv(makeClientRequest)
                                         , ClientM
                                         , Scheme(Http)
                                         , client
                                         , defaultMakeClientRequest
                                         , mkClientEnv
                                         , runClientM
                                         )
import           Servant.Client.Core     ( addHeader )

-- | A request to the RPC server: its @act@, and @add@ if it has arguments.
data RPC m a where
  -- | @GET \/16\/rpc@. The reply's signature has been checked: see 'verifyRPCResponse'.
  CallRPC :: Text -> Maybe Text -> RPC m ByteString
  -- | @GET \/16\/dl@, the gallery downloader's endpoint, whose replies carry their
  -- own signature ('parseMetadata' checks it).
  CallDownloader :: Text -> Maybe Text -> RPC m ByteString

makeSem ''RPC

runRPC :: Members '[ Embed IO, Error HathError, Reader ClientConfig ] r => Sem (RPC : r) a -> Sem r a
runRPC = interpret $ \case
  CallRPC act add
    -- get_cert answers with the PKCS#12 file itself, which is not a signed reply
    | act == "get_cert" -> request "/16/rpc" act add
    | otherwise -> do
      cfg <- ask @ClientConfig
      verifyRPCResponse (encodeUtf8 (key cfg)) <$> request "/16/rpc" act add
  CallDownloader act add -> request "/16/dl" act add

-- | Run RPC requests on their own, outside the web server.
runRPCIO
  :: ClientConfig
  -> Sem '[ RPC, Log Message, Reader ClientConfig, Error HathError, Embed IO, Final IO ] a
  -> IO (Either HathError a)
runRPCIO cfg
  = runFinal
  . embedToFinal @IO
  . errorToIOFinal @HathError
  . runReader cfg
  . runLogAction @IO richMessageAction
  . runRPC

type RPCAPI
  = QueryParam "act" Text
  :> QueryParam "cid" Text
  :> QueryParam "add" Text
  :> QueryParam "acttime" Text
  :> QueryParam "actkey" Text
  :> QueryParam "clientbuild" Text
  :> Get '[ RPCText, OctetStream ] ByteString

-- | What the RPC server sends: bytes labelled @text\/plain@.
data RPCText

instance Accept RPCText where
  contentType _ = "text" // "plain" /: ( "charset", "utf-8" )

instance MimeUnrender RPCText ByteString where
  mimeUnrender _ = Right . toStrict

rpcClient
  :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM ByteString
rpcClient = client (Proxy @RPCAPI)

-- | The RPC server, unless @HATH_RPC_HOST@ (@host@ or @host:port@) names another;
-- the tests and hath-bench point us at theirs that way.
rpcServer :: IO ( String, Int )
rpcServer = lookupEnv "HATH_RPC_HOST" <&> \case
  Nothing       -> ( "rpc.hentaiathome.net", 80 )
  Just override -> case break (== ':') override of
    ( host, ':' : port ) -> ( host, fromMaybe 80 (readMaybe port) )
    ( host, _ ) -> ( host, 80 )

rpcTimeoutMicros :: Int
rpcTimeoutMicros = 60 * 1000000

-- | One GET to the RPC server, signed with the client key and the current time.
request :: Members '[ Embed IO, Error HathError, Reader ClientConfig ] r
        => String
        -> Text
        -> Maybe Text
        -> Sem r ByteString
request endpoint act add = do
  cfg <- ask @ClientConfig
  reply <- embed $ do
    ( host, port ) <- rpcServer
    now <- systemSeconds <$> getSystemTime
    manager <- getGlobalManager
    runClientM
      (rpcClient
         (Just act)
         (Just (clientId cfg))
         add
         (Just (show now))
         (Just (rpcActionKey cfg act (fromMaybe "" add) now))
         (Just clientBuild))
      (mkClientEnv manager (BaseUrl Http host port endpoint))
      { makeClientRequest = \baseUrl req -> do
          httpRequest <- defaultMakeClientRequest baseUrl (foldr withHeader req hathHeaders)
          pure httpRequest { responseTimeout = responseTimeoutMicro rpcTimeoutMicros }
      }
  either (throw . TransportError) pure reply
  where
    withHeader ( name, value ) = addHeader name (decodeUtf8 @Text value)

-- | The payload of an OK reply.
payload :: Member (Error HathError) r => ByteString -> Sem r [ ByteString ]
payload = fromEither . rpcPayload

-- | Whether the RPC server answers. Any reply counts, whatever its status.
serverStat :: Members '[ RPC, Log Message ] r => Sem r Bool
serverStat = do
  available <- not . BS.null <$> callRPC "server_stat" Nothing
  logInfo $ "Server availability: " <> show available
  pure available

-- | Log in and receive the settings assigned to us.
clientLogin :: Members '[ RPC, Error HathError, Log Message ] r => Sem r HathSettings
clientLogin = do
  logInfo "Logging in"
  parseSettings defaultHathSettings <$> (payload =<< callRPC "client_login" Nothing)

-- | Tell the server we are listening. False unless it answers OK.
clientStart :: Member RPC r => Sem r Bool
clientStart = isRight . rpcPayload <$> callRPC "client_start" Nothing

-- | Tell the server we are going away.
clientStop :: Member RPC r => Sem r ()
clientStop = void $ callRPC "client_stop" Nothing

-- | The heartbeat.
stillAlive :: Member RPC r => Sem r ()
stillAlive = void $ callRPC "still_alive" Nothing

-- | The settings as the server has them now, applied onto the ones we hold.
clientSettings :: Members '[ RPC, Error HathError ] r => HathSettings -> Sem r HathSettings
clientSettings current = parseSettings current <$> (payload =<< callRPC "client_settings" Nothing)

-- | Our TLS certificate. The server sends a PKCS#12 file whose password is the client key.
getCertificate
  :: Members '[ RPC, Error HathError, Reader ClientConfig ] r => Sem r ( CertificateChain, PrivKey )
getCertificate = do
  p12 <- callRPC "get_cert" Nothing
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

-- | Files added to the blacklist in the last so many seconds.
getBlacklist :: Members '[ RPC, Error HathError ] r => Int -> Sem r [ Text ]
getBlacklist seconds
  = fmap decodeUtf8 <$> (payload =<< callRPC "get_blacklist" (Just (show seconds)))

-- | URLs a file of our static ranges can be fetched from (@srfetch@), given
-- the @fileindex@ and @xres@ of the request that asked for it.
fileSources
  :: Members '[ RPC, Error HathError ] r => FileURI -> ByteString -> ByteString -> Sem r [ ByteString ]
fileSources uri fileIndex xres
  = payload
  =<< callRPC "srfetch" (Just $ decodeUtf8 $ BS.intercalate ";" [ fileIndex, xres, renderFileURIBytes uri ])

-- | The next gallery queued for download, or why there is none
-- (@NO_PENDING_DOWNLOADS@, or a list that was refused).
nextGallery :: Members '[ RPC, Reader ClientConfig ] r => Sem r (Either Text GalleryMetadata)
nextGallery = do
  cfg <- ask @ClientConfig
  parseMetadata (encodeUtf8 (key cfg)) <$> callDownloader "fetchqueue" Nothing

-- | URLs a gallery file can be downloaded from (@dlfetch@); the last argument
-- counts the attempts already made.
galleryFileSources :: Members '[ RPC, Error HathError ] r
                   => GalleryMetadata
                   -> GalleryFile
                   -> Int
                   -> Sem r [ ByteString ]
galleryFileSources gallery file attempt
  = payload
  =<< callRPC
    "dlfetch"
    (Just
       [i|#{galleryID gallery};#{galleryFilePage file};#{galleryFileIndex file};#{galleryFileXRes file};#{attempt}|])

-- | Report sources that served a file with the wrong hash.
reportDownloadFailures :: Member RPC r => [ Text ] -> Sem r ()
reportDownloadFailures failures = void $ callRPC "dlfails" (Just $ T.intercalate ";" failures)

-- | Tell the server every file of the gallery is on disk.
completeGallery :: Members '[ RPC, Log Message ] r => GalleryMetadata -> Sem r ()
completeGallery gallery = do
  logInfo "Notifying server that a gallery task has been completed"
  void $ callDownloader "fetchqueue" (Just [i|#{galleryID gallery};#{galleryMinXRes gallery}|])
