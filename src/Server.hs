{-# LANGUAGE DataKinds #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( startServer
  , ServerAction(..)
  , makeApplication
  , mkServerLoopContext
  , CacheRunner(..)
  , IPMap
  , KeystampMap
  , GalleryTask(..)
  ) where

import           Colog                                ( Message
                                                      , Severity(Info, Warning)
                                                      , richMessageAction
                                                      )
import           Colog.Polysemy                       ( Log, runLogAction )

import           Control.Concurrent                   ( forkIO, threadDelay )
import           Control.Concurrent.Suspend           ( mDelay )
import           Control.Concurrent.Timer             ( repeatedTimer, stopTimer )
import           Control.Exception                    ( finally, try )

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as BSC
import           Data.ByteString.Builder              ( byteString )
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Lazy.Char8           as LBS8
import qualified Data.HashMap.Strict                  as HashMap
import qualified Data.Map.Strict                      as Map
import           Data.String.Interpolate              ( i )
import qualified Data.Text                            as Text
import           Data.Time.Clock.System               ( SystemTime(systemSeconds), getSystemTime )
import           Data.X509                            ( CertificateChain, PrivKey )

import           Database.SQLite.Simple               ( withConnection )

import           HathNetwork.Genesis
import           HathNetwork.RPC                      ( RPC
                                                      , notifyGalleryCompletion
                                                      , runRPC
                                                      , runRPCIO
                                                      , stillAlive
                                                      )

import           Interface.API                        ( API
                                                      , EHentaiAPI
                                                      , ServerCommand(..)
                                                      , api
                                                      , downloadGalleryFile
                                                      , fetchBlacklist
                                                      , nextGalleryTask
                                                      , renderServerCommand
                                                      , runEHentaiAPI
                                                      , runEHentaiAPIIO
                                                      , startListening
                                                      , stopListening
                                                      )

import           LegacyCiphers

import qualified Mason.Builder                        as BD

import           Middleware

import qualified Network.HTTP.Client                  as HC
import           Network.HTTP.Client                  ( Manager
                                                      , brRead
                                                      , newManager
                                                      , parseRequest
                                                      , withResponse
                                                      )
import           Network.HTTP.Client.TLS              ( tlsManagerSettings )
import           Network.HTTP.Simple                  ( getResponseBody, getResponseStatusCode )
import qualified Network.HTTP.Types                   as HT
import           Network.HTTP.Types                   ( hContentLength
                                                      , hContentType
                                                      , status200
                                                      , status404
                                                      )
import           Network.TLS                          ( Credentials(Credentials), Version(..) )
import           Network.TLS.Extra                    ( ciphersuite_all )
import           Network.Wai                          ( Middleware
                                                      , Request(requestMethod)
                                                      , responseFile
                                                      , responseLBS
                                                      , responseStream
                                                      )
import           Network.Wai.Handler.Warp             ( defaultSettings, setPort )
import           Network.Wai.Handler.WarpTLS          ( OnInsecure(AllowInsecure)
                                                      , TLSSettings(..)
                                                      , runTLS
                                                      , tlsSettingsSni
                                                      )
import           Network.Wai.Middleware.RealIp        ( realIpHeader )
import           Network.Wai.Middleware.RequestLogger ( logStdout )

import           Polysemy                             ( Embed
                                                      , Members
                                                      , Sem
                                                      , embed
                                                      , embedToFinal
                                                      , runFinal
                                                      )
import           Polysemy.Async                       ( Async, asyncToIOFinal )
import           Polysemy.Error                       ( Error, errorToIOFinal, mapError, throw )
import           Polysemy.KVStore                     ( KVStore )
import           Polysemy.Reader                      ( Reader, ask, runReader )

import           Relude                               hiding ( Reader, ask, get, runReader )

import           Servant
import           Servant.Client                       ( ClientError )
import qualified Servant.Types.SourceT                as Source

import           SettingM                             ( SettingM(..)
                                                      , getSettings
                                                      , runSettingM
                                                      , updateSettings
                                                      )

import           Stats                                ( Stats
                                                      , StatsEnv(..)
                                                      , addDlBytes
                                                      , addDownload
                                                      , addUpload
                                                      , incDlFile
                                                      , incDlTask
                                                      , incServed
                                                      , newStatsEnv
                                                      , readPrometheus
                                                      , runStats
                                                      )

import           Storage.Database                     ( FileRecord(fileRecordBytes)
                                                      , initializeDB
                                                      , runCache
                                                      )
import           Storage.Filesystem                   ( newFsCache, runCacheFilesystem, runEvictor )
import           Storage.Locate
import           Storage.R2                           ( mkR2Connection, runCacheR2 )

import           System.Directory                     ( doesFileExist )
import           System.IO                            ( hPutStrLn )

import           Types                                ( CacheBackend(..)
                                                      , ClientConfig
                                                      , ClientConfig(..)
                                                      , FileURI(fileExt, fileSize)
                                                      , GalleryFile(..)
                                                      , GalleryMetadata(..)
                                                      , HathSettings(..)
                                                      , RPCError
                                                      , StorageResult(..)
                                                      , hentaiHeader
                                                      , parseFileURI
                                                      )

import           UnliftIO                             ( TChan
                                                      , getMonotonicTime
                                                      , newTChanIO
                                                      , race
                                                      , readTChan
                                                      , replicateConcurrently
                                                      , timeout
                                                      , withAsync
                                                      , writeTChan
                                                      )

import           Utils                                ( bufferSending
                                                      , hash
                                                      , log
                                                      , lookupParam
                                                      , parseURLParams
                                                      , writeFileAtomic
                                                      )

maxTimeDrift :: Int64
maxTimeDrift = 600

-- | Official Settings.MAX_KEY_TIME_DRIFT, used for servercmd.
maxCommandTimeDrift :: Int64
maxCommandTimeDrift = 300

-- | Whole-download cap for one threaded_proxy_test request.
-- ponytail: one total timeout; the official client uses 10 s connect + 60 s per read.
testTimeoutMicros :: Int
testTimeoutMicros = 60 * 1000000

data ServerAction = Reload | Cert | GracefulShutdown

data GalleryTask = GalleryTask

-- | Server runtime environment containing all channels for inter-thread communication.
-- This bundles channels used by the server loop to coordinate actions and tasks.
data ServerChannels
  = ServerChannels
  { serverActionChan :: !(TChan ServerAction)
    -- ^ Channel for receiving server control commands (reload, cert refresh, shutdown)
  , galleryTaskChan  :: !(TChan GalleryTask)
    -- ^ Channel for coordinating gallery download tasks
  }

-- | Server runtime state containing all mutable variables.
-- These TVars are shared across threads and updated during server operation.
data ServerState
  = ServerState { serverSettings :: !(TVar HathSettings)
                  -- ^ Dynamic server settings that can be updated via RPC
                , serverStatsEnv :: !StatsEnv
                  -- ^ Prometheus metrics environment (gauges, counters, histograms)
                }

-- | Server configuration flags controlling middleware behavior.
data ServerFlags
  = ServerFlags { disableRateLimit  :: !Bool
                  -- ^ If True, bypass IP-based rate limiting middleware
                , trustProxyHeaders :: !Bool
                  -- ^ If True, trust X-Forwarded-For headers for real client IP
                }

-- | Server loop context bundling all runtime dependencies.
-- This groups all parameters needed by the server loop to reduce parameter count
-- and improve code clarity.
data ServerLoopContext
  = ServerLoopContext
  { loopConfig      :: !ClientConfig
    -- ^ Static client configuration (client ID, key, cache backend, etc.)
  , loopChannels    :: !ServerChannels
    -- ^ Communication channels for server actions and gallery tasks
  , loopState       :: !ServerState
    -- ^ Mutable server state (settings, rate limits, metrics)
  , loopFlags       :: !ServerFlags
    -- ^ Configuration flags controlling middleware behavior
  , loopCacheRunner :: !CacheRunner
    -- ^ Cache backend interpreter (SQLite, R2, or Filesystem)
  }

mkServerLoopContext
  :: ClientConfig
  -> TVar HathSettings
  -> TChan ServerAction
  -> TChan GalleryTask
  -> Bool
  -> Bool
  -> CacheRunner
  -> StatsEnv
  -> ServerLoopContext
mkServerLoopContext cfg settings action gallery disableRL trustProxy cacheRunner statsEnv
  = ServerLoopContext
  { loopConfig      = cfg
  , loopChannels    = ServerChannels
      { serverActionChan = action
      , galleryTaskChan  = gallery
      }
  , loopState       = ServerState
      { serverSettings = settings
      , serverStatsEnv = statsEnv
      }
  , loopFlags       = ServerFlags
      { disableRateLimit  = disableRL
      , trustProxyHeaders = trustProxy
      }
  , loopCacheRunner = cacheRunner
  }

-- Server implementation
server :: Members
         '[ Embed IO
          , Error ServerError
          , Error RPCError
          , EHentaiAPI
          , Reader ClientConfig
          , RPC
          , Locate
          , Reader (TChan ServerAction)
          , Reader (TChan GalleryTask)
          , Stats
          , Log Message
          , Genesis
          , SettingM
          , Async
          ]
         r
       => ServerT API (Sem r)
server
  = faviconHandler
  :<|> robotsHandler
  :<|> resourceHandler
  :<|> statsHandler
  :<|> serverCmdHandler
  :<|> testHandler
  :<|> adminSettingsHandler
  :<|> Tagged rawHandler
  where
    faviconHandler
      = throw $ err301 { errHeaders = [ ( "Location", "https://e-hentai.org/favicon.ico" ) ] }

    robotsHandler = return "User-agent: *\nDisallow: /"

    resourceHandler
      (encodeUtf8 -> fileid)
      (parseURLParams . encodeUtf8 -> opts)
      (encodeUtf8 -> filename)
      req
      respond
      -- answer HEAD like any other path instead of opening an upstream body nobody reads
      | requestMethod req == "HEAD" = embed $ respond $ responseLBS status200 hentaiHeader ""
      | otherwise = do
      currentTime <- embed getSystemTime
      cfg <- ask @ClientConfig
      when (abs (timestamp - systemSeconds currentTime) > maxTimeDrift)
        $ throw err403 { errBody = "Your time is out of sync. Please update your system time." }
      when (answer /= challange cfg) $ throw err403 { errBody = "Invalid key." }
      locateResource
        LocateURI { locateURIFilename = filename, locateURI = uri, locateURIOptions = opts }
        >>= \case
          Nothing -> throw err404
          Just (Stored (Redirect url)) -> throw
            $ err302
            { errHeaders = [ ( "Location", url ), ( "Cache-Control", "public, max-age=600" ) ] }
          Just (Stored (LocalFile path)) -> do
            incServed
            addUpload (fileSize uri)
            -- Warp streams the file and sets Content-Length from it
            embed $ respond $ responseFile status200 [ ( hContentType, mimeType ) ] path Nothing
          Just (Stored (Record (fileRecordBytes -> bs))) -> do
            incServed
            addUpload (BS.length bs)
            embed $ respond $ responseLBS status200 (fileHeaders (BS.length bs)) (LBS.fromStrict bs)
          -- cache miss: relay upstream bytes as they arrive, like the official proxy mode
          Just (Streamed relay) -> do
            incServed
            addUpload (fileSize uri)
            embed
              $ respond
              $ responseStream status200 (fileHeaders (fileSize uri))
              $ \write flush -> relay (\chunk -> write (byteString chunk) >> flush)
      where
        fileHeaders :: Int -> [ HT.Header ]
        fileHeaders len = [ ( hContentType, mimeType ), ( hContentLength, show len ) ]

        mimeType = case fileExt uri of
          "jpg" -> "image/jpeg"
          "png" -> "image/png"
          "gif" -> "image/gif"
          "mp4" -> "video/mp4"
          "wbm" -> "video/webm"
          "wbp" -> "image/webp"
          "avf" -> "image/avif"
          "jxl" -> "image/jxl"
          _     -> "application/octet-stream"

        uri = parseFileURI fileid

        ( timestamp :: Int64, answer )
          = let
              rs        = fromMaybe "" (Map.lookup "keystamp" opts)
              ( t, rk ) = BSC.span (/= '-') rs
            in 
              ( fromIntegral $ maybe 0 fst $ BSC.readInteger t, BS.drop 1 rk )

        {-# INLINE challange #-}
        -- [i|#{timestamp}-#{fileid}-#{key cfg}-hotlinkthis|]
        challange :: ClientConfig -> ByteString
        challange cfg
          = BS.take 10
          $ hash
          $ BD.toStrictByteString
            (BD.int64Dec timestamp
             <> "-"
             <> BD.byteString fileid
             <> "-"
             <> BD.textUtf8 (key cfg)
             <> "-hotlinkthis")

    serverCmdHandler command rawAdditional commandTime commandKey
      = checkCommandKey
      >> case command of
        StillAlive        -> plainText "I feel FANTASTIC and I'm still alive"
        ThreadedProxyTest -> let
            args = do
              hostname <- lookupParam "hostname" additional
              protocol <- lookupParam "protocol" additional <|> return "http"
              port <- lookupParam "port" additional
              testSize <- readInt @Int64 =<< lookupParam "testsize" additional
              testCount <- readInt =<< lookupParam "testcount" additional
              testTime <- readInt @Int64 =<< lookupParam "testtime" additional
              testKey <- lookupParam "testkey" additional
              return
                ( testCount
                , testSize
                  -- [i|#{protocol}://#{hostname}:#{port}/t/#{testSize}/#{testTime}/#{testKey}/randomkeyhere|]
                , LBS8.unpack
                  $ BD.toLazyByteString
                    (BD.byteString protocol
                     <> "://"
                     <> BD.byteString hostname
                     <> ":"
                     <> BD.byteString port
                     <> "/t/"
                     <> BD.int64Dec testSize
                     <> "/"
                     <> BD.int64Dec testTime
                     <> "/"
                     <> BD.byteString testKey
                     <> "/0")
                )
          in 
            case args of
              Nothing -> throw err403
              Just ( testCount, testSize, url ) -> do
                mgr <- embed $ newManager tlsManagerSettings
                case parseRequest url of
                  Nothing  -> throw err403
                  Just req -> do
                    results <- embed $ replicateConcurrently testCount $ runTest testSize req mgr
                    let succeeded = catMaybes results
                    addUpload (testCount * fromIntegral testSize)
                    -- official format: OK:<successful tests>-<total millis of successful tests>
                    plainText [i|OK:#{length succeeded}-#{sum succeeded}|]
        SpeedTest         -> let
            testSize
              = maybe
                1000000
                (fromIntegral . fst)
                (BSC.readInteger =<< lookupParam "testsize" additional)
          in 
            do
              addDownload testSize
              return
                $ addHeader @"Content-Length" testSize
                $ Source.fromStepT
                $ bufferSending testSize
        RefreshSettings   -> do
          log Info "Refreshing settings"
          updateSettings =<< fetchSettings =<< getSettings
          plainText ""
        StartDownloader   -> do
          chan <- ask @(TChan GalleryTask)
          log Info "Starting gallery downloader"
          void $ embed $ forkIO $ do
            atomically $ writeTChan chan GalleryTask
          plainText ""
        RefreshCerts      -> do
          log Info "Refreshing certificates"
          chan <- ask @(TChan ServerAction)
          void $ embed $ forkIO $ do
            threadDelay (1000000 * 2)
            atomically $ writeTChan chan Cert
          plainText ""
      where
        additional = parseURLParams (encodeUtf8 rawAdditional)

        -- same check as the official client: only the RPC server knows our key
        checkCommandKey = do
          currentTime <- embed getSystemTime
          cfg <- ask @ClientConfig
          let expected
                = hash @Text
                $ encodeUtf8 @Text
                  [i|hentai@home-servercmd-#{renderServerCommand command}-#{rawAdditional}-#{clientId cfg}-#{commandTime}-#{key cfg}|]
          when (abs (fromIntegral commandTime - systemSeconds currentTime) > maxCommandTimeDrift
                || commandKey /= expected)
            $ throw err403

        -- A test succeeds only if the whole body arrives with status 200 within the
        -- timeout (official: FileDownloader + getContentLength() >= testsize).
        runTest :: Int64 -> HC.Request -> Manager -> IO (Maybe Int64)
        runTest testSize req mgr = do
          start <- getMonotonicTime
          ok <- timeout testTimeoutMicros
            $ try @SomeException
            $ withResponse req mgr
            $ \resp -> let
                drain n
                  | n <= 0 = pure True
                  | otherwise = do
                    chunk <- brRead (getResponseBody resp)
                    -- an empty chunk is EOF: the body was shorter than promised
                    if BS.null chunk
                      then pure False
                      else drain (n - fromIntegral (BS.length chunk))
              in
                if getResponseStatusCode resp == 200
                  then drain testSize
                  else pure False
          end <- getMonotonicTime
          pure $ case ok of
            Just (Right True) -> Just (truncate ((end - start) * 1000))
            _ -> Nothing

        {-# INLINE readInt #-}
        {-# SPECIALISE readInt :: ByteString -> Maybe Int #-}
        {-# SPECIALISE readInt :: ByteString -> Maybe Int64 #-}
        readInt :: Num a => ByteString -> Maybe a
        readInt = fmap (fromIntegral . fst) . BSC.readInteger

        {-# INLINE plainText #-}
        plainText t
          = pure
          $ addHeader @"Content-Length" (BS.length t)
          $ Source.fromStepT
          $ Source.Yield t Source.Stop

    statsHandler = readPrometheus

    testHandler testSize testTime (encodeUtf8 @_ @ByteString -> testKey) _ = do
      currentTime <- embed getSystemTime
      cfg <- ask @ClientConfig
      when (abs (testTime - systemSeconds currentTime) > maxTimeDrift) $ throw err403
      when (testKey /= challange cfg) $ throw err403
      addUpload testSize
      return $ addHeader @"Content-Length" testSize $ Source.fromStepT $ bufferSending testSize
      where
        {-# INLINE challange #-}
        -- [i|hentai@home-speedtest-#{testSize}-#{testTime}-#{clientId cfg}-#{key cfg}|]
        challange :: ClientConfig -> ByteString
        challange cfg
          = hash
          $ BD.toStrictByteString
            ("hentai@home-speedtest-"
             <> BD.intDec testSize
             <> "-"
             <> BD.int64Dec testTime
             <> "-"
             <> BD.textUtf8 (clientId cfg)
             <> "-"
             <> BD.textUtf8 (key cfg))

    adminSettingsHandler = show <$> getSettings

    rawHandler req k = case requestMethod req of
      -- Handle HEAD requests with 200 OK and empty body
      "HEAD" -> k $ responseLBS status200 hentaiHeader ""

      _      -> k $ responseLBS status404 [] ""

-- | Set up periodic timers for various housekeeping tasks
tictok :: ClientConfig -> IO (IO ())
tictok config = do
  -- heartbeat
  heartbeatHandle <- repeatedTimer (void $ runRPCIO config stillAlive) (mDelay 1)

  -- resource blacklist
  blacklistHandle <- repeatedTimer (do
                                      res <- runRPCIO config (fetchBlacklist 43200)
                                      print res) (mDelay 36)

  return $ do
    stopTimer heartbeatHandle
    stopTimer blacklistHandle

notifyStart :: ClientConfig -> IO ()
notifyStart config = psi
  where
    psi = runRPCIO config startListening >>= \case
      Right (Right True) -> return ()
      _ -> threadDelay 1000000 >> psi

-- | Abstract cache runner - wraps the cache effect interpreter
-- This allows us to switch between SQLite and R2 backends at runtime
newtype CacheRunner
  = CacheRunner
  { runCacheWith
      :: forall r a. Members '[ Embed IO, Log Message, Reader ClientConfig, Error RPCError ] r
      => Sem (KVStore FileURI StorageResult : r) a
      -> Sem r a
  }

-- Create the WAI application with rate limiting
makeApplication :: ServerLoopContext -> Application
makeApplication serverCxt
  = finalMiddleware
  $ normalizeAcceptMiddleware
  $ tracingConnections statsEnv
  $ tracingTimeUsage statsEnv
  $ withHentaiHeaders
  $ serve api (hoistServer api interpretServer server)
  where
    ServerLoopContext
      { loopConfig = cfg
      , loopChannels = ServerChannels { serverActionChan = action, galleryTaskChan = gallery }
      , loopState = ServerState { serverSettings = settings, serverStatsEnv = statsEnv }
      , loopFlags = ServerFlags { disableRateLimit = disableRL, trustProxyHeaders = trustProxy }
      , loopCacheRunner = cacheRunner
      } = serverCxt

    applyRealIp :: Middleware
    applyRealIp
      = if trustProxy
        then realIpHeader "X-Forwarded-For"
        else id

    applyRateLimit :: Middleware
    applyRateLimit
      = if disableRL
        then id
        else rateLimitMiddleware isRpcServer

    -- dual-stack sockets would show RPC servers as ::ffff:a.b.c.d; Warp listens on IPv4 here
    isRpcServer ip = (\st -> Just ip `elem` (parseIPv4 <$> rpcServerIPs st)) <$> readTVarIO settings

    finalMiddleware :: Middleware
    finalMiddleware = applyRealIp . applyRateLimit

    interpretServer :: Sem _ a -> Handler a
    interpretServer
      = Handler
      . ExceptT
      . runFinal
      . asyncToIOFinal
      . embedToFinal @IO
      . errorToIOFinal @ServerError
      . mapError @RPCError (const err500)
      . mapError @ClientError (const err500)
      . mapError @SomeException (const err500)
      . runSettingM settings
      . runReader cfg
      . runReader action
      . runReader statsEnv
      . runReader gallery
      . runLogAction @IO @Message richMessageAction
      . runEHentaiAPI
      . runCacheWith cacheRunner
      . runStats
      . runLocate
      . runGenesis
      . runRPC

-- | Serve until told to stop. The RPC server is told we are listening, then
-- control commands are handled while the TLS server runs:
--
-- * 'Cert' fetches new certificates and swaps them in for the next handshakes;
--   the listener and open transfers carry on.
-- * 'GracefulShutdown' tells the RPC server we stop, then exits.
-- * 'Reload' exits so the supervisor restarts us.
serverLoop :: ServerLoopContext
           -- ^ Server runtime context (config, channels, state, flags, cache)
           -> ( CertificateChain, PrivKey )
           -- ^ Initial TLS certificates
           -> IO ()
serverLoop ctx certs = do
  let ServerLoopContext { loopConfig = cfg
                        , loopChannels = ServerChannels { serverActionChan = chan }
                        , loopState = ServerState { serverSettings = settings }
                        } = ctx

      -- returns the first command that ends the server
      handleCommands credsRef = atomically (readTChan chan) >>= \case
        Cert   -> refreshCerts cfg >>= writeIORef credsRef >> handleCommands credsRef
        action -> pure action

  credsRef <- newIORef certs
  st <- readTVarIO settings
  result <- withAsync (notifyStart cfg) $ \_ -> race (handleCommands credsRef)
    $ runTLS
      -- asked on every handshake (SNI or not), so a swapped certificate applies at once
      ((tlsSettingsSni $ \_ -> Credentials . pure <$> readIORef credsRef)
       { onInsecure         = AllowInsecure
       , tlsCiphers         = ciphersuite_all
           ++ [ cipher_ECDHE_ECDSA_AES128CBC_SHA
              , cipher_ECDHE_RSA_AES128CBC_SHA
              , cipher_ECDHE_RSA_AES256CBC_SHA
              ]
       , tlsAllowedVersions = [ TLS10, TLS11, TLS12, TLS13 ]
       })
      (setPort (clientPort st) defaultSettings)
    $ logStdout (makeApplication ctx)

  case result of
    Left GracefulShutdown -> gracefulShutdown cfg
    Left _ -> exitSuccess
    Right _ -> error "Server terminated unexpectedly"

-- | Perform graceful shutdown by notifying RPC server and exiting.
--
-- This function repeatedly calls 'stopListening' until the RPC server
-- acknowledges the shutdown request, then exits the process cleanly.
gracefulShutdown :: ClientConfig -> IO a
gracefulShutdown cfg = go (3 :: Int)
  where
    -- The official client sends client_stop once. Never spin when the RPC server is unreachable.
    go n = runEHentaiAPIIO cfg stopListening >>= \case
      Left _
        | n > 1 -> threadDelay 1000000 >> go (n - 1)
      _ -> exitSuccess

-- | Refresh TLS certificates with exponential backoff retry logic.
--
-- Attempts to fetch new certificates from the Genesis RPC endpoint.
-- On failure, retries up to 3 times with exponential backoff (2^n seconds).
-- After 3 failures, triggers graceful shutdown.
--
-- This is called when the server receives a 'Cert' action, typically
-- triggered by the periodic certificate refresh timer or manual server command.
refreshCerts :: ClientConfig -> IO ( CertificateChain, PrivKey )
refreshCerts cfg = retry 0
  where
    retry :: Int -> IO ( CertificateChain, PrivKey )
    retry retries = runGenesisIO cfg fetchCertificate >>= \case
      Right (Right newCerts) -> return newCerts
      e -> do
        putStrLn $ "Failed to refresh certs: " <> show e
        if retries >= 3
          then do
            putStrLn "Giving up after 3 retries"
            gracefulShutdown cfg
          else do
            let delaySeconds = 2 ^ retries :: Int
            putStrLn
              $ "Retrying in "
              <> show delaySeconds
              <> " seconds (attempt "
              <> show (retries + 1)
              <> "/3)"
            threadDelay (1000000 * delaySeconds)
            retry (retries + 1)

startServer :: ClientConfig
            -> HathSettings
            -> ( CertificateChain, PrivKey )
            -> TChan ServerAction
            -> Bool
            -> Bool
            -> IO ()
startServer config settings certs chan disableRateLimit trustProxyHeaders = do
  ssVar <- newTVarIO settings
  galleryHandler <- newTChanIO
  statsEnv <- newStatsEnv
  void $ forkIO $ forever $ do
    GalleryTask <- atomically $ readTChan galleryHandler
    result <- try @SomeException
      $ runFinal
      $ embedToFinal @IO
      $ errorToIOFinal @ClientError
      $ errorToIOFinal @RPCError
      $ runReader config
      $ runReader statsEnv
      $ runStats
      $ runLogAction @IO @Message richMessageAction
      $ runEHentaiAPI
      $ runRPC startsDownloader
    case result of
      Left e  -> hPutStrLn stderr $ "Gallery downloader error: " <> show e
      Right _ -> return ()
  print =<< runRPCIO config (fetchBlacklist 259200)
  let ctx runner
        = ServerLoopContext
        { loopConfig      = config
        , loopChannels    = ServerChannels
            { serverActionChan = chan, galleryTaskChan = galleryHandler }
        , loopState       = ServerState { serverSettings = ssVar, serverStatsEnv = statsEnv }
        , loopFlags       = ServerFlags
            { disableRateLimit = disableRateLimit, trustProxyHeaders = trustProxyHeaders }
        , loopCacheRunner = runner
        }
  case cacheBackend config of
    CacheBackendSQLite     -> do
      withConnection (Text.unpack $ cachePath config) $ \conn -> do
        initializeDB conn
        let cacheRunner = CacheRunner { runCacheWith = runCache conn }
        stopTictok <- tictok config
        finally (serverLoop (ctx cacheRunner) certs) stopTictok
    CacheBackendR2         -> do
      case r2Config config of
        Nothing    -> error "cacheBackend=r2 but r2Config is missing"
        Just r2Cfg -> do
          r2ConnResult <- mkR2Connection r2Cfg
          case r2ConnResult of
            Left err     -> error $ "Failed to initialize R2: " <> err
            Right r2Conn -> do
              let cacheRunner = CacheRunner { runCacheWith = runCacheR2 r2Conn }
              stopTictok <- tictok config
              finally (serverLoop (ctx cacheRunner) certs) stopTictok
    CacheBackendFilesystem -> do
      fs <- newFsCache (Text.unpack $ cachePath config)
      let cacheRunner = CacheRunner { runCacheWith = runCacheFilesystem fs }
      stopTictok <- tictok config
      withAsync (runEvictor fs (diskLimitBytes <$> readTVarIO ssVar))
        $ \_ -> finally (serverLoop (ctx cacheRunner) certs) stopTictok
  where
    startsDownloader = do
      metadata <- nextGalleryTask
      incDlTask
      case metadata of
        Left "NO_PENDING_DOWNLOADS" -> log Info "No gallery task available"
        Left reason -> log Warning [i|Gallery download refused: #{reason}|]
        Right md -> do
          log Info [i|Starting download for gallery #{galleryID md}|]
          complete <- mapM (verifyAndDownload md) (galleryFileList md)
          when (and complete) (notifyGalleryCompletion md)

    verifyAndDownload md f = do
      let filePath :: FilePath
            = [i|download/#{galleryID md}/#{galleryFileName f}.#{galleryFileExt f}|]
      existingBytes <- embed $ do
        existing <- doesFileExist filePath
        if existing
          then BS.readFile filePath
          else return BS.empty
      -- a file generated on demand has no known hash; like the official client, keep it
      if not (BS.null existingBytes) && maybe True (== hash existingBytes) (galleryFileHash f)
        then log Info [i|#{galleryFileName f}.#{galleryFileExt f} already verified, skipping|]
          >> pure True
        else downloadGalleryFile md f >>= \case
          Nothing    -> do
            log Warning [i|Failed to download #{galleryFileName f}.#{galleryFileExt f}|]
            pure False
          Just bytes -> do
            log Info [i|Downloaded #{galleryFileName f}.#{galleryFileExt f}|]
            incDlFile
            addDlBytes (BS.length bytes)
            embed $ writeFileAtomic filePath bytes
            pure True


