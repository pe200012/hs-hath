{-# LANGUAGE DataKinds #-}

{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module API
    (   -- * H@H Client Exposed API
      API
    , api
    , ServerCommand(..)
    , DynCT
    , WithDynamicContentType(..)
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
    , fetchResource
    , runEHentaiAPIIO
    ) where

import           Crypto.Store.PKCS12      ( readP12FileFromMemory
                                          , recover
                                          , recoverAuthenticated
                                          , toCredential
                                          , toProtectionPassword
                                          )

import qualified Data.ByteString.Char8    as BSC
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString.Short    as SBS
import           Data.String.Interpolate  ( i )
import qualified Data.Text                as T
import           Data.Time.Clock.System   ( SystemTime(systemSeconds), getSystemTime )
import           Data.X509                ( CertificateChain, PrivKey )

import           Hash                     ( hash )

import           Network.HTTP.Client      ( Request(responseTimeout, host, requestHeaders)
                                          , Response(responseBody)
                                          , defaultManagerSettings
                                          , newManager
                                          , parseRequest
                                          , responseTimeoutMicro
                                          )
import           Network.HTTP.Simple      ( httpLbs )

import           Polysemy
import           Polysemy.Error           ( Error, errorToIOFinal, throw )
import           Polysemy.Reader          ( Reader, ask, runReader )
import           Polysemy.State           ( State, modify, runState )

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

import           Types                    ( ClientConfig
                                          , FileURI(..)
                                          , GalleryFile(..)
                                          , GalleryMetadata(galleryMinXRes, galleryID)
                                          , HathSettings
                                          , MkClientConfig(..)
                                          , RPCError(CertificateFailure)
                                          , emptyMetadata
                                          , hentaiHeader
                                          , parseMetadata
                                          , parseRPCResponse
                                          , parseRPCResponse'
                                          , parseSettings
                                          )

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
instance FromHttpApiData ServerCommand where
    parseUrlPiece = \case
        "still_alive" -> Right StillAlive
        "threaded_proxy_test" -> Right ThreadedProxyTest
        "speed_test" -> Right SpeedTest
        "refresh_settings" -> Right RefreshSettings
        "start_downloader" -> Right StartDownloader
        "refresh_certs" -> Right RefreshCerts
        cmd -> Left $ "Invalid command: " <> cmd

data DynCT
    deriving ( Typeable )

instance MimeRender DynCT ByteString where
    mimeRender _ = LBS.fromStrict

instance Accept DynCT where
    contentType _ = ""

data WithDynamicContentType
    = WithDynamicContentType
    { contentType :: {-# UNPACK #-} !ByteString, content :: {-# UNPACK #-} !ByteString }

instance MimeRender DynCT WithDynamicContentType where
    mimeRender _ = LBS.fromStrict . content

instance AllCTRender '[ DynCT ] WithDynamicContentType where
    handleAcceptH _ _ (WithDynamicContentType ct content)
        = Just ( LBS.fromStrict ct, LBS.fromStrict content )

data SpeedTest
    deriving ( Typeable )

instance MimeRender SpeedTest ByteString where
    mimeRender _ = LBS.fromStrict

instance Accept SpeedTest where
    contentType _ = "text/html; charset=iso-8859-1"

-- floskell-disable
-- API type definitions
type API
    = Get '[PlainText] NoContent
    :<|> "favicon.ico" :> Get '[PlainText] NoContent
    :<|> "robots.txt" :> Get '[PlainText] Text
    :<|> "h"
        :> Capture "info" Text
        :> Capture "opts" Text
        :> Capture "filename" Text
        :> Get '[DynCT] (Headers '[Header "Content-Length" Int] WithDynamicContentType)
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

runEHentaiAPIIO
    :: ClientConfig
    -> Sem '[ EHentaiAPI, Reader ClientConfig, Error ClientError, Embed IO, Final IO ] a
    -> IO (Either ClientError a)
runEHentaiAPIIO cfg
    = runFinal . embedToFinal . errorToIOFinal @ClientError . runReader cfg . runEHentaiAPI

runEHentaiAPI :: forall a r. Members '[ Embed IO, Error ClientError, Reader ClientConfig ] r
              => Sem (EHentaiAPI ': r) a
              -> Sem r a
runEHentaiAPI m = do
    cfg <- ask @ClientConfig
    manager <- embed $ newManager defaultManagerSettings
    currentTime <- embed (systemSeconds <$> getSystemTime)
    let k :: RPCParams -> String -> Sem r ByteString
        k params endpoint
            = either throw pure
            =<< embed
                (runClientM
                     (ehAPIM
                          params { acttime     = Just (show currentTime)
                                 , clientbuild = Just (decodeUtf8 $ version cfg)
                                 , actkey      = Just $ makeKey params cfg currentTime
                                 , cid         = Just (decodeUtf8 $ clientId cfg)
                                 })
                     (mkClientEnv manager (BaseUrl Http "rpc.hentaiathome.net" 80 endpoint))
                     { makeClientRequest = \baseUrl req -> do
                           servantReq <- defaultMakeClientRequest
                               baseUrl
                               (foldr (uncurry (addHeader @Text)) req hentaiHeader)
                           pure servantReq { responseTimeout = responseTimeoutMicro rpcTimeout } })
        {-# INLINE k #-}
    interpret (\case
                   EhRPC params     -> k params "/15/rpc"
                   EhGallery params -> k params "/15/dl") m
  where
    rpcTimeout :: Int
    rpcTimeout = 60 * 1000000  -- 60 seconds in microseconds

    makeKey :: RPCParams -> ClientConfig -> Int64 -> Text
    makeKey params cfg time
        = let
            act'      = fromMaybe "" (act params)
            add'      = fromMaybe "" (add params)
            clientId' = SBS.fromShort $ clientId cfg
            key'      = SBS.fromShort $ key cfg
            in 
                hash [i|hentai@home-#{act'}-#{add'}-#{clientId'}-#{time}-#{key'}|]

checkServerStatus :: Member EHentaiAPI r => Sem r Bool
checkServerStatus = do
    res <- ehRPC emptyRPCParams { act = Just "server_stat" }
    case parseRPCResponse res of
        Left _  -> return False
        Right _ -> return True

heartbeat :: Member EHentaiAPI r => Sem r ()
heartbeat = void $ ehRPC emptyRPCParams { act = Just "still_alive" }

startListening :: Member EHentaiAPI r => Sem r ()
startListening = void $ ehRPC emptyRPCParams { act = Just "client_start" }

stopListening :: Member EHentaiAPI r => Sem r ()
stopListening = void $ ehRPC emptyRPCParams { act = Just "client_stop" }

login :: Members '[ EHentaiAPI, Error RPCError ] r => Sem r HathSettings
login = do
    x <- ehRPC emptyRPCParams { act = Just "client_login" }
    parseSettings <$> parseRPCResponse' x

downloadCertificates :: Members '[ EHentaiAPI, Error RPCError, Reader ClientConfig ] r
                     => Sem r ( CertificateChain, PrivKey )
downloadCertificates = do
    bytes <- ehRPC emptyRPCParams { act = Just "get_cert" }
    cfg <- ask @ClientConfig
    fromPkcs12 cfg bytes
  where
    fromPkcs12 (SBS.fromShort . key -> passcode) bytes = case maybeCred of
        Left err       -> throw $ CertificateFailure $ show err
        Right Nothing  -> throw $ CertificateFailure "no credential"
        Right (Just c) -> pure c
      where
        maybeCred = do
            p12 <- readP12FileFromMemory bytes
            ( _, pkcs12 ) <- recoverAuthenticated passcode p12
            recover (toProtectionPassword passcode) (toCredential pkcs12)

getSettings :: Members '[ EHentaiAPI, Error RPCError ] r => Sem r HathSettings
getSettings = do
    x <- ehRPC emptyRPCParams { act = Just "client_settings" }
    parseSettings <$> parseRPCResponse' x

nextGalleryTask :: Members '[ EHentaiAPI ] r => Sem r (Maybe GalleryMetadata)
nextGalleryTask = do
    m <- parseMetadata <$> ehGallery emptyRPCParams { act = Just "fetchqueue" }
    if m == emptyMetadata
        then return Nothing
        else return $ Just m

completeGalleryTask :: Members '[ EHentaiAPI ] r => GalleryMetadata -> Sem r ()
completeGalleryTask metadata
    = void
    $ ehGallery
        emptyRPCParams { act = Just "fetchqueue"
                       , add = Just [i|#{galleryID metadata};#{galleryMinXRes metadata}|]
                       }

downloadGalleryFile
    :: forall r. Members '[ EHentaiAPI, Reader ClientConfig, Error RPCError, Embed IO ] r
    => GalleryMetadata
    -> GalleryFile
    -> Sem r (Maybe ByteString)
downloadGalleryFile metadata file = do
    ( failures, maybeContent ) <- runState [] (operate 0)
    case maybeContent of
        Just content -> return $ Just content
        Nothing      -> do
            reportFailures failures
            return Nothing
  where
    download :: String -> Sem (State [ Text ] ': r) (Maybe ByteString)
    download url = case parseRequest url of
        Nothing  -> pure Nothing
        Just req -> do
            bytes <- embed $ LBS.toStrict . responseBody <$> httpLbs @IO req
            if hash bytes /= galleryFileHash file
                then do
                    modify
                        @[ Text ]
                        ([i|#{host req}-#{galleryFileIndex file}-#{galleryFileXRes file}|] :)
                    pure Nothing
                else return $ Just bytes

    operate :: Word8 -> Sem (State [ Text ] ': r) (Maybe ByteString)
    operate retries
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
                then operate retries -- we did not get a valid URL, might be a network error
                else asum <$> traverse (download . BSC.unpack) urls

reportFailures :: Members '[ EHentaiAPI ] r => [ Text ] -> Sem r ()
reportFailures reports
    = void $ ehRPC emptyRPCParams { act = Just "dlfails", add = Just $ T.intercalate ";" reports }

fetchResource :: Members '[ EHentaiAPI, Reader ClientConfig, Error RPCError, Embed IO ] r
              => FileURI
              -> ( ByteString, ByteString )
              -> Sem r (Maybe ByteString)
fetchResource fileURI ( fileIndex, xres ) = do
    urls <- parseRPCResponse'
        =<< ehRPC
            emptyRPCParams
            { act = Just "srfetch", add = Just [i|#{fileIndex};#{xres};#{fileURI}|] }
    cfg <- ask @ClientConfig
    asum
        <$> traverse
            (download
                 [i|#{clientId cfg}-#{hash @ByteString (SBS.fromShort(key cfg) <> show fileURI)}|]
             . BSC.unpack)
            urls
  where
    download token url = case parseRequest url of
        Nothing  -> pure Nothing
        Just req -> do
            bytes <- embed
                $ LBS.toStrict . responseBody
                <$> httpLbs @IO req { requestHeaders = ( "Hath-Request", token ) : hentaiHeader }
            if hash bytes /= fileHash fileURI
                then pure Nothing
                else return $ Just bytes