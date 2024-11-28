{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module RPCAction ( module RPCAction ) where

import           Colog                      ( Message, WithLog, logInfo )

import           Control.Monad.Catch        ( MonadThrow, throwM )

import           Crypto.Store.PKCS12        ( readP12FileFromMemory
                                            , recover
                                            , recoverAuthenticated
                                            , toCredential
                                            , toProtectionPassword
                                            )

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short      as SBS
import qualified Data.HashSet               as HashSet
import           Data.List                  ( lookup )
import           Data.String.Interpolate    ( i )
import           Data.Time.Clock            ( UTCTime, getCurrentTime, nominalDiffTimeToSeconds )
import           Data.Time.Clock.POSIX      ( utcTimeToPOSIXSeconds )
import           Data.X509                  ( Certificate, CertificateChain, PrivKey )

import           Database                   ( FileRecord(..), lookupFile, storeFile )
import           Database.SQLite.Simple     ( Connection )

import           Network.HTTP.Client        ( Request
                                            , Response(responseBody)
                                            , method
                                            , parseRequest
                                            , requestHeaders
                                            , responseTimeout
                                            , responseTimeoutMicro
                                            , setQueryString
                                            )
import           Network.HTTP.Simple        ( getResponseStatus, httpLbs, setRequestIgnoreStatus )
import qualified Network.HTTP.Types.Status  as HTTP

import           Polysemy

import           RPC

import           Relude

import           Types                      hiding ( rpcBaseURL )

import           Utils                      ( hathHash )

data RPCAction m a where
    ServerStat :: RPCAction m [ ByteString ]
    StillAlive :: RPCAction m ()
    ClientStart :: RPCAction m ()
    ClientStop :: RPCAction m ()
    ClientLogin :: RPCAction m ()
    DownloadCertificate :: RPCAction m Certificate
    FetchQueue :: RPCAction m [ ByteString ]
    FetchGalleryFile :: RPCAction m [ ByteString ]
    FetchResource :: RPCAction m [ ByteString ]

makeSem ''RPCAction

{-
-- | Timeout in microseconds (1 minute)
rpcTimeout :: Int
rpcTimeout = 60 * 1000000  -- 60 seconds in microseconds

-- Base RPC URL from settings
rpcBaseURL :: String -> String
rpcBaseURL baseURL = baseURL <> "/15/rpc"

-- | RPC request parameters
data RPCRequest
    = RPCRequest { rpcAction      :: !ByteString        -- ^ The action to perform (act)
                 , rpcClientId    :: !ByteString        -- ^ Client ID (cid)
                 , rpcAdditional  :: !ByteString        -- ^ Additional parameters (add)
                 , rpcTime        :: !UTCTime           -- ^ Request time (acttime)
                 , rpcKey         :: !ByteString        -- ^ Client key (actkey)
                 , rpcClientBuild :: !ByteString        -- ^ Client version (clientbuild)
                 }
    deriving ( Show, Generic )

-- | Build an HTTP request with RPC parameters
buildRPCRequest :: Request       -- ^ Base request
                -> RPCRequest    -- ^ RPC parameters
                -> Request
buildRPCRequest baseReq RPCRequest { .. }
    = setQueryString queryParams
    $ baseReq { method = "GET", responseTimeout = responseTimeoutMicro rpcTimeout }
  where
    -- Convert UTCTime to POSIX timestamp
    timestamp
        = BS.pack . show @_ @Integer . floor
        $ nominalDiffTimeToSeconds
        $ utcTimeToPOSIXSeconds rpcTime

    -- Build query parameters
    queryParams :: [ ( ByteString, Maybe ByteString ) ]
    queryParams
        = [ ( "act", Just rpcAction )
          , ( "cid", Just rpcClientId )
          , ( "add"
            , if BS.null rpcAdditional
                  then Nothing
                  else Just rpcAdditional
            )
          , ( "acttime", Just timestamp )
          , ( "actkey", Just rpcKey )
          , ( "clientbuild", Just rpcClientBuild )
          ]

buildRequestM :: ( MonadIO m, MonadThrow m, WithLog env Message m )
              => ClientConfig
              -> String
              -> ByteString
              -> ByteString
              -> m [ ByteString ]
buildRequestM config baseURL rpcAction rpcAdditional = do
    rpcTime <- liftIO getCurrentTime
    let rpcClientId    = SBS.fromShort $ clientId config
        rpcKey         = SBS.fromShort $ key config
        rpcClientBuild = SBS.fromShort $ version config
    baseReq <- liftIO $ parseRequest baseURL
    let req
            = buildRPCRequest
                baseReq
                RPCRequest
                { rpcAction, rpcClientId, rpcAdditional, rpcTime, rpcKey, rpcClientBuild }
    logInfo (show req)
    res <- liftIO $ httpLbs req
    case parseRPCResponse (responseBody res) >>= getPayload of
        Left err      -> throwM err
        Right payload -> return payload

-- | Get server statistics
serverStat :: ( MonadIO m, MonadThrow m, WithLog env Message m )
           => ClientConfig
           -> String
           -> m [ ByteString ]
serverStat config baseURL = buildRequestM config (rpcBaseURL baseURL) "server_stat" mempty

-- | Keep-alive request
stillAlive :: ( MonadIO m, MonadThrow m, WithLog env Message m )
           => ClientConfig
           -> String
           -> m [ ByteString ]
stillAlive config baseURL = buildRequestM config (rpcBaseURL baseURL) "still_alive" mempty

-- | Start client session
--
-- Always login first before starting a session!
clientStart :: ( MonadIO m, MonadThrow m, WithLog env Message m )
            => ClientConfig
            -> String
            -> m [ ByteString ]
clientStart config baseURL = buildRequestM config (rpcBaseURL baseURL) "client_start" mempty

-- | Stop client session
clientStop :: ( MonadIO m, MonadThrow m, WithLog env Message m )
           => ClientConfig
           -> String
           -> m [ ByteString ]
clientStop config baseURL = buildRequestM config (rpcBaseURL baseURL) "client_stop" mempty

-- | Login client
clientLogin :: ( MonadIO m, MonadThrow m, WithLog env Message m )
            => ClientConfig
            -> String
            -> m [ ByteString ]
clientLogin config baseURL = buildRequestM config (rpcBaseURL baseURL) "client_login" mempty

-- | Download client certificate
downloadCertificate :: ( MonadIO m, MonadThrow m, WithLog env Message m )
                    => ClientConfig
                    -> String
                    -> m [ ByteString ]
downloadCertificate config baseURL = buildRequestM config (rpcBaseURL baseURL) "get_cert" mempty

-- | Convert PKCS12 certificate to certificate chain and private key
fromPkcs12 :: ClientConfig -> LBS.ByteString -> ( CertificateChain, PrivKey )
fromPkcs12 (SBS.fromShort . key -> pass) bytes = case cred of
    Left err -> error $ show err
    Right c  -> c
  where
    cred = do
        p12 <- readP12FileFromMemory (BS.concat $ LBS.toChunks bytes)
        ( _, pkcs12 ) <- recoverAuthenticated pass p12
        maybeCred <- recover (toProtectionPassword pass) (toCredential pkcs12)
        case maybeCred of
            Just c  -> return c
            Nothing -> error "no credential"

-- | Get client settings from server
hathSettings :: ( MonadIO m, MonadThrow m, WithLog env Message m )
             => String
             -> ClientConfig
             -> m HathSettings
hathSettings baseURL cfg = do
    foldl' (\s kv -> let
                ( k, rest ) = BS.span (/= '=') kv
                v           = BS.drop 1 rest
                in
                    case ( k, readMaybe @Int64 (BS.unpack v) ) of
                        ( "host", _ ) -> s { clientHost = SBS.toShort v }
                        ( "port", Just p ) -> s { clientPort = fromIntegral p }
                        ( "throttle_bytes", Just bytes ) -> s { throttleBytes = bytes }
                        ( "disklimit_bytes", Just bytes ) -> s { diskLimitBytes = bytes }
                        ( "diskremaining_bytes", Just bytes ) -> s { diskRemainingBytes = bytes }
                        ( "static_ranges", _ ) -> s
                            { staticRanges = HashSet.fromList $ SBS.toShort <$> BS.split ';' v }
                        _ -> s) defaultHathSettings
        <$> buildRequestM cfg (rpcBaseURL baseURL) "client_settings" mempty

-- | Get metadata for gallery files
galleryMetadata :: ( MonadIO m, MonadThrow m, WithLog env Message m )
                => String
                -> ClientConfig
                -> Maybe ( ByteString, ByteString )
                -> m [ ByteString ]
galleryMetadata baseURL cfg opt
    = buildRequestM cfg (baseURL <> "/15/dl") "fetchqueue" (case opt of
                                                                Nothing -> ""
                                                                Just ( gid, minxres )
                                                                    -> [i|#{gid};#{minxres}|])

-- | Fetch a specific gallery file
galleryFetch :: ( MonadIO m, MonadThrow m, WithLog env Message m )
             => String
             -> ClientConfig
             -> Int
             -> GalleryFile
             -> Int
             -> m [ ByteString ]
galleryFetch baseURL cfg gid file retries
    = buildRequestM
        cfg
        (baseURL <> "/15/rpc")
        "dlfetch"
        [i|#{gid};#{galleryFilePage file};#{galleryFileIndex file};#{galleryFileXRes file};#{retries}|]

-- | Fetch a specific resource from the server
fetchResource :: ( MonadIO m, MonadThrow m, WithLog env Message m )
              => String            -- ^ Base URL
              -> ClientConfig     -- ^ Client configuration
              -> ByteString       -- ^ File index
              -> ByteString       -- ^ X resolution
              -> ByteString       -- ^ File ID
              -> m [ ByteString ]
fetchResource baseURL cfg fileIndex xres fileId
    = buildRequestM cfg (rpcBaseURL baseURL) "srfetch" [i|#{fileIndex};#{xres};#{fileId}|]

-- | Locate a resource either in cache or fetch from remote
locateResource :: ( MonadIO m, MonadThrow m, WithLog env Message m )
               => Connection          -- ^ Database connection
               -> ClientConfig       -- ^ Client configuration
               -> String            -- ^ Base URL
               -> ByteString        -- ^ Filename
               -> FileURI        -- ^ File URI
               -> [ ( ByteString, ByteString ) ]  -- ^ Options (fileindex and xres)
               -> HashSet ShortByteString    -- ^ Static ranges
               -> m (Maybe ByteString)
locateResource conn cfg baseURL filename uri opts staticRanges = do
    if inStaticRange uri staticRanges
        then do
            logInfo [i|Locating resource for #{uri}|]
            -- Try cache first
            -- cached <- liftIO $ lookupFile conn (show uri)
            -- TODO
            cached <- undefined
            case cached of
                Just record -> pure $ Just $ fileRecordBytes record
                Nothing     -> case ( lookup "fileindex" opts, lookup "xres" opts ) of
                    ( Just fi, Just xres ) -> do
                        -- Fetch from remote
                        results <- fetchResource baseURL cfg fi xres (show uri)
                        mapM (pullResource cfg uri) results >>= \case
                            []          -> pure Nothing
                            (bytes : _) -> do
                                -- Store in cache
                                -- liftIO
                                --     $ storeFile
                                --         conn
                                --         FileRecord
                                --         { fileRecordLRUCounter = 1
                                --         , fileRecordS4
                                --               = decodeUtf8 $ BS.take 4 $ fileHash uri
                                --         , fileRecordFileId     = show uri
                                --         , fileRecordFileName   = Just $ decodeUtf8 filename
                                --         , fileRecordBytes      = bytes
                                --         }
                                -- TODO
                                pure $ Just bytes
                    _ -> pure Nothing
        else pure Nothing
  where
    inStaticRange fid = HashSet.member (SBS.toShort $ BS.take 4 $ fileHash fid)

-- | Pull resource from a specific URL
pullResource :: ( MonadIO m, MonadThrow m )
             => ClientConfig
             -> FileURI        -- ^ File ID
             -> ByteString        -- ^ Resource URL
             -> m ByteString
pullResource cfg fid uri = do
    initReq <- parseRequest $ BS.unpack uri
    let reqToken = makeRequestToken cfg fid
        req
            = setRequestIgnoreStatus
            $ initReq { method = "GET", requestHeaders = [ ( "Hath-Request", reqToken ) ] }
    res <- httpLbs req
    if getResponseStatus res == HTTP.status200
        then pure $ LBS.toStrict $ responseBody res
        else throwM $ RequestFailure "Resource fetch failed"

-- | Make request token for authentication
makeRequestToken :: ClientConfig -> FileURI -> ByteString
makeRequestToken cfg uri
    = let
        cid  = SBS.fromShort $ clientId cfg
        key_ = SBS.fromShort $ key cfg
        in
            [i|#{cid}-#{hathHash (key_ <> show uri)}|]

-}