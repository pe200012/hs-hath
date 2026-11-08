{-# LANGUAGE DataKinds #-}

{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Interface.API
  (   -- * H@H Client Exposed API
    API
  , api
  , ServerCommand(..)
    -- * EHentai RPC
  , EHentaiAPI(..)
  , ehRPC
  , RPCParams(..)
  , emptyRPCParams
  , runEHentaiAPI
  , checkServerStatus
  , heartbeat
  , startListening
  , stopListening
  , login
  , downloadCertificates
  , getSettings
  , nextGalleryTask
  , completeGalleryTask
  , downloadGalleryFile
  , reportFailures
  , openResource
  , fetchBlacklist
  , runEHentaiAPIIO
  , renderServerCommand
  ) where

import           Crypto.Store.PKCS12      ( readP12FileFromMemory
                                          , recover
                                          , recoverAuthenticated
                                          , toCredential
                                          , toProtectionPassword
                                          )

import qualified Data.ByteString.Char8    as BSC
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.List                as List
import           Data.String.Interpolate  ( i )
import qualified Data.Text                as T
import           Data.Time.Clock.System   ( SystemTime(systemSeconds), getSystemTime )
import           Data.X509                ( CertificateChain, PrivKey )

import qualified Mason.Builder            as BD

import           Network.HTTP.Client      ( BodyReader
                                          , HttpException
                                          , Manager
                                          , Request(responseTimeout, host, requestHeaders)
                                          , Response(responseBody, responseHeaders, responseStatus)
                                          , responseClose
                                          , responseOpen
                                          , defaultManagerSettings
                                          , newManager
                                          , parseRequest
                                          , responseTimeoutMicro
                                          )
import           Network.HTTP.Media       ( (//), (/:) )
import           Network.HTTP.Client.TLS  ( getGlobalManager )
import           Network.HTTP.Simple      ( httpLbs )
import           Network.HTTP.Types       ( hContentLength )
import qualified Network.HTTP.Types       as HT

import           Polysemy
import           Polysemy.Error           ( Error, errorToIOFinal, throw )
import           Polysemy.Operators
import           Polysemy.Reader          ( Reader, ask, runReader )
import           Polysemy.State           ( modify, runState )

import           Relude                   hiding ( Reader
                                                 , State
                                                 , ask
                                                 , modify
                                                 , runReader
                                                 , runState
                                                 )

import           Servant                  hiding ( addHeader )
import           Servant.API.ContentTypes ( AllCTRender(handleAcceptH) )
import           Servant.Client           ( BaseUrl(BaseUrl)
                                          , ClientEnv(makeClientRequest)
                                          , ClientError
                                          , ClientM
                                          , Scheme(Http)
                                          , client
                                          , defaultMakeClientRequest
                                          , mkClientEnv
                                          , runClientM
                                          )
import           Servant.Client.Core      ( addHeader )

import           System.IO.Unsafe         ( unsafePerformIO )

import           Types                    ( ClientConfig
                                          , ClientConfig(..)
                                          , FileURI(..)
                                          , GalleryFile(..)
                                          , GalleryMetadata(galleryMinXRes, galleryID)
                                          , HathSettings
                                          , RPCError(CertificateFailure)
                                          , RPCResponse(statusCode)
                                          , clientBuild
                                          , defaultHathSettings
                                          , hentaiHeader
                                          , parseMetadata
                                          , parseRPCResponse
                                          , parseRPCResponse'
                                          , parseSettings
                                          , renderFileURIBytes
                                          , verifyRPCResponse
                                          )

import           UnliftIO                 ( try )

import           Utils                    ( firstJustM, hash )

-- | Available server commands
data ServerCommand
  = StillAlive        -- ^ Heartbeat request
  | ThreadedProxyTest -- ^ Multi-threaded speed test
  | SpeedTest        -- ^ Speed test
  | RefreshSettings  -- ^ Reloads hath configuration
  | StartDownloader  -- ^ Initiates gallery downloader
  | RefreshCerts     -- ^ Updates SSL certificates
  deriving ( Show, Eq )

-- Custom type conversion for ServerCommand
serverCommandNames :: [ ( Text, ServerCommand ) ]
serverCommandNames
  = [ ( "still_alive", StillAlive )
    , ( "threaded_proxy_test", ThreadedProxyTest )
    , ( "speed_test", SpeedTest )
    , ( "refresh_settings", RefreshSettings )
    , ( "start_downloader", StartDownloader )
    , ( "refresh_certs", RefreshCerts )
    ]

instance FromHttpApiData ServerCommand where
  parseUrlPiece cmd = maybeToRight ("Invalid command: " <> cmd) (List.lookup cmd serverCommandNames)

-- | Wire name of a command, needed to verify the servercmd signature.
renderServerCommand :: ServerCommand -> Text
renderServerCommand c = maybe "" fst (find ((== c) . snd) serverCommandNames)

data SpeedTest
  deriving ( Typeable )

instance MimeRender SpeedTest ByteString where
  {-# INLINE mimeRender #-}
  mimeRender _ = LBS.fromStrict

instance Accept SpeedTest where
  {-# INLINE contentTypes #-}
  contentTypes _
    = "text" // "html" /: ( "charset", "iso-8859-1" )
    :| [ "image" // "gif", "image" // "jpeg", "*" // "*" /: ( "q", ".2" ) ]

-- floskell-disable
-- API type definitions
type API
    =    "favicon.ico" :> Get '[PlainText] NoContent
    :<|> "robots.txt" :> Get '[PlainText] Text
    :<|> "h"
        :> Capture "info" Text
        :> Capture "opts" Text
        :> Capture "filename" Text
        :> RawM
    :<|> "metrics" :> Get '[PlainText] Text
    :<|> "servercmd"
        :> Capture "command" ServerCommand
        :> Capture "additional" Text
        :> Capture "time" Int
        :> Capture "key" Text
        :> StreamGet NoFraming OctetStream (Headers '[Header "Content-Length" Int] (SourceIO ByteString))
    :<|> "t"
        :> Capture "testsize" Int
        :> Capture "testtime" Int64
        :> Capture "testkey" Text
        :> Capture "nothing" Text
        :> StreamGet NoFraming SpeedTest (Headers '[Header "Content-Length" Int] (SourceIO ByteString))
    :<|> "admin"
        :> "settings"
        :> Get '[PlainText] Text
    :<|> Raw  -- This will catch all other routes including HEAD requests
-- floskell-enable

-- API specification
api :: Proxy API
api = Proxy

-- floskell-disable
type EHAPI =
       QueryParam "act" Text
  :>   QueryParam "cid" Text
  :>   QueryParam "add" Text
  :>   QueryParam "acttime" Text
  :>   QueryParam "actkey" Text
  :>   QueryParam "clientbuild" Text
  :>   Get '[PlainText,OctetStream] ByteString
-- floskell-enable

instance MimeUnrender PlainText ByteString where
  {-# INLINE mimeUnrender #-}
  mimeUnrender _ = mimeUnrender (Proxy @OctetStream)

data RPCParams
  = RPCParams { act         :: {-# UNPACK #-} !(Maybe Text)
              , cid         :: {-# UNPACK #-} !(Maybe Text)
              , add         :: {-# UNPACK #-} !(Maybe Text)
              , acttime     :: {-# UNPACK #-} !(Maybe Text)
              , actkey      :: {-# UNPACK #-} !(Maybe Text)
              , clientbuild :: {-# UNPACK #-} !(Maybe Text)
              }

emptyRPCParams :: RPCParams
emptyRPCParams
  = RPCParams { act         = Nothing
              , cid         = Nothing
              , add         = Nothing
              , acttime     = Nothing
              , actkey      = Nothing
              , clientbuild = Nothing
              }

ehAPI :: Proxy EHAPI
ehAPI = Proxy

ehAPIM :: RPCParams -> ClientM ByteString
ehAPIM RPCParams { .. } = client ehAPI act cid add acttime actkey clientbuild

data EHentaiAPI m a where
  EhRPC :: RPCParams -> EHentaiAPI m ByteString
  EhGallery :: RPCParams -> EHentaiAPI m ByteString

makeSem ''EHentaiAPI

{-# NOINLINE ehHttpManager #-}
ehHttpManager :: Manager
ehHttpManager = unsafePerformIO $ newManager defaultManagerSettings

{-# INLINE runEHentaiAPIIO #-}
runEHentaiAPIIO :: ClientConfig
                -> [ EHentaiAPI, Reader ClientConfig, Error ClientError, Embed IO, Final IO ] @> a
                -> IO (Either ClientError a)
runEHentaiAPIIO cfg
  = runFinal . embedToFinal . errorToIOFinal @ClientError . runReader cfg . runEHentaiAPI

{-# INLINE runEHentaiAPI #-}
runEHentaiAPI :: forall a r. Members [ Embed IO, Error ClientError, Reader ClientConfig ] r
              => EHentaiAPI : r @> a
              -> r @> a
runEHentaiAPI m = do
  cfg <- ask @ClientConfig
  currentTime <- embed (systemSeconds <$> getSystemTime)
  -- Internal: Override RPC host via HATH_RPC_HOST environment variable
  -- Format: "host" or "host:port" (default port: 80)
  rpcHostOverride <- embed (lookupEnv "HATH_RPC_HOST" :: IO (Maybe String))
  let ( rpcHostName, rpcPort ) = case rpcHostOverride of
        Nothing      -> ( "rpc.hentaiathome.net", 80 )
        Just hostStr -> case break (== ':') hostStr of
          ( h, ':' : p ) -> ( h, fromMaybe 80 (readMaybe p) )
          ( h, _ )       -> ( h, 80 )
  let k :: RPCParams -> String -> Sem r ByteString
      k params endpoint
        = either throw pure
        =<< embed
          (runClientM
             (ehAPIM
                params { acttime     = Just (show currentTime)
                       , clientbuild = Just clientBuild
                       , actkey      = Just $ makeKey params cfg currentTime
                       , cid         = Just (clientId cfg)
                       })
             (mkClientEnv ehHttpManager (BaseUrl Http rpcHostName rpcPort endpoint))
             { makeClientRequest = \baseUrl req -> do
                 servantReq <- defaultMakeClientRequest
                   baseUrl
                   (foldr (uncurry (addHeader @Text)) req hentaiHeader)
                 pure servantReq { responseTimeout = responseTimeoutMicro rpcTimeout } })
      {-# INLINE k #-}
  interpret (\case
               -- get_cert is the raw PKCS#12 file, not a signed reply
               EhRPC params
                 | act params == Just "get_cert" -> k params "/16/rpc"
                 | otherwise -> verifyRPCResponse (encodeUtf8 (key cfg)) <$> k params "/16/rpc"
               -- the gallery list carries its own signature, checked by parseMetadata
               EhGallery params -> k params "/16/dl") m
  where
    rpcTimeout :: Int
    rpcTimeout = 60 * 1000000  -- 60 seconds in microseconds

    makeKey :: RPCParams -> ClientConfig -> Int64 -> Text
    makeKey params cfg time
      = let
          act'      = fromMaybe "" (act params)
          add'      = fromMaybe "" (add params)
          clientId' = clientId cfg
          key'      = key cfg
        in 
          -- [i|hentai@home-#{act'}-#{add'}-#{clientId'}-#{time}-#{key'}|]
          hash
          $ BD.toStrictByteString
            ("hentai@home-"
             <> BD.textUtf8 act'
             <> "-"
             <> BD.textUtf8 add'
             <> "-"
             <> BD.textUtf8 clientId'
             <> "-"
             <> BD.int64Dec time
             <> "-"
             <> BD.textUtf8 key')

{-# INLINE checkServerStatus #-}
-- checkServerStatus :: Member EHentaiAPI r => Sem r Bool
checkServerStatus :: [ EHentaiAPI, Error RPCError ] >@> Bool
checkServerStatus = do
  res <- ehRPC emptyRPCParams { act = Just "server_stat" }
  case parseRPCResponse res of
    Left _  -> return False
    Right _ -> return True

{-# INLINE heartbeat #-}
heartbeat :: EHentaiAPI -@> ()
heartbeat = void $ ehRPC emptyRPCParams { act = Just "still_alive" }

{-# INLINE startListening #-}
startListening :: EHentaiAPI -@> Bool
startListening = do
  res <- ehRPC emptyRPCParams { act = Just "client_start" }
  case parseRPCResponse res of
    Left _  -> return False
    Right x -> return $ statusCode x == "OK"

{-# INLINE stopListening #-}
stopListening :: EHentaiAPI -@> Bool
stopListening = do
  res <- ehRPC emptyRPCParams { act = Just "client_stop" }
  case parseRPCResponse res of
    Left _  -> return False
    Right _ -> return True

{-# INLINE login #-}
login :: [ EHentaiAPI, Error RPCError ] >@> HathSettings
login = do
  x <- ehRPC emptyRPCParams { act = Just "client_login" }
  parseSettings defaultHathSettings <$> parseRPCResponse' x

downloadCertificates
  :: [ EHentaiAPI, Error RPCError, Reader ClientConfig ] >@> ( CertificateChain, PrivKey )
downloadCertificates = do
  bytes <- ehRPC emptyRPCParams { act = Just "get_cert" }
  cfg <- ask @ClientConfig
  fromPkcs12 cfg bytes
  where
    fromPkcs12 (encodeUtf8 . key -> passcode) bytes = case maybeCred of
      Left err       -> throw $ CertificateFailure $ show err
      Right Nothing  -> throw $ CertificateFailure "no credential"
      Right (Just c) -> pure c
      where
        maybeCred = do
          p12 <- readP12FileFromMemory bytes
          ( _, pkcs12 ) <- recoverAuthenticated passcode p12
          recover (toProtectionPassword passcode) (toCredential pkcs12)

{-# INLINE getSettings #-}
-- | Refreshed settings, applied onto the current ones.
getSettings :: HathSettings -> [ EHentaiAPI, Error RPCError ] >@> HathSettings
getSettings current = do
  x <- ehRPC emptyRPCParams { act = Just "client_settings" }
  parseSettings current <$> parseRPCResponse' x

{-# INLINE nextGalleryTask #-}
-- | Left says why there is nothing to download (NO_PENDING_DOWNLOADS, or a refused list).
nextGalleryTask :: [ EHentaiAPI, Reader ClientConfig ] >@> Either Text GalleryMetadata
nextGalleryTask = do
  cfg <- ask @ClientConfig
  parseMetadata (encodeUtf8 (key cfg)) <$> ehGallery emptyRPCParams { act = Just "fetchqueue" }

{-# INLINE completeGalleryTask #-}
completeGalleryTask :: GalleryMetadata -> EHentaiAPI -@> ()
completeGalleryTask metadata
  = void
  $ ehGallery
    emptyRPCParams
    { act = Just "fetchqueue", add = Just [i|#{galleryID metadata};#{galleryMinXRes metadata}|] }

downloadGalleryFile
  :: GalleryMetadata
  -> GalleryFile
  -> [ EHentaiAPI, Reader ClientConfig, Error RPCError, Embed IO ] >@> Maybe ByteString
downloadGalleryFile metadata file = do
  ( failures, maybeContent ) <- runState @[ Text ] [] (operate 0)
  case maybeContent of
    Just content -> return $ Just content
    Nothing      -> do
      reportFailures failures
      return Nothing
  where
    download url = case parseRequest url of
      Nothing  -> pure Nothing
      Just req -> embed (try @IO @HttpException $ LBS.toStrict . responseBody <$> httpLbs @IO req)
        >>= \case
          Left _      -> pure Nothing
          Right bytes -> do
            if maybe False (/= hash bytes) (galleryFileHash file)
              then do
                modify
                  @[ Text ]
                  ([i|#{host req}-#{galleryFileIndex file}-#{galleryFileXRes file}|] :)
                pure Nothing
              else return $ Just bytes

    operate (retries :: Int)
      | retries > 3 = return Nothing
      | otherwise = do
        urls <- parseRPCResponse'
          =<< ehRPC
            emptyRPCParams
            { act = Just "dlfetch"
            , add = Just
                [i|#{galleryID metadata};#{galleryFilePage file};#{galleryFileIndex file};#{galleryFileXRes file};#{retries}|]
            }

        if null urls
          then operate (retries + 1) -- we did not get a valid URL, might be a network error
          else do
            -- `traverse` would execute every fallback download eagerly.
            -- We need monadic short-circuiting to stop after the first success.
            firstJustM (download . BSC.unpack) urls >>= maybe (operate (retries + 1)) (pure . Just)

{-# INLINE reportFailures #-}
reportFailures :: [ Text ] -> EHentaiAPI -@> ()
reportFailures reports
  = void $ ehRPC emptyRPCParams { act = Just "dlfails", add = Just $ T.intercalate ";" reports }

-- | Ask the RPC server where a static-range file lives and open the first source
-- that answers 200 with the expected length, as the official ProxyFileDownloader
-- does. The caller streams the body and must close the response.
openResource :: FileURI
             -> ( ByteString, ByteString )
             -> [ EHentaiAPI, Reader ClientConfig, Error RPCError, Embed IO ] >@> Maybe (Response BodyReader)
openResource fileURI ( fileIndex, xres ) = do
  urls <- parseRPCResponse'
    =<< ehRPC
      emptyRPCParams { act = Just "srfetch", add = Just [i|#{fileIndex};#{xres};#{fileURI}|] }
  cfg <- ask @ClientConfig
  let token = [i|#{clientId cfg}-#{hash @ByteString $ encodeUtf8 (key cfg) <> renderFileURIBytes fileURI}|]
  embed $ do
    mgr <- getGlobalManager
    firstJustM (open mgr token . BSC.unpack) urls
  where
    open mgr token url = case parseRequest url of
      Nothing  -> pure Nothing
      Just req -> try @IO @HttpException
        (responseOpen req { requestHeaders = ( "Hath-Request", token ) : hentaiHeader } mgr)
        >>= \case
          Right resp
            | HT.statusCode (responseStatus resp) == 200
            , List.lookup hContentLength (responseHeaders resp) == Just (show (fileSize fileURI)) ->
              pure (Just resp)
            | otherwise -> responseClose resp $> Nothing
          -- connection error or wrong answer: firstJustM tries the next source
          Left _ -> pure Nothing

fetchBlacklist :: Int -> [ EHentaiAPI, Error RPCError ] >@> [ Text ]
fetchBlacklist deltaTime = do
  res <- ehRPC emptyRPCParams { act = Just "get_blacklist", add = Just $ show deltaTime }
  fmap decodeUtf8 <$> parseRPCResponse' res
