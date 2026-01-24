{-# LANGUAGE DataKinds #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( startServer
  , ServerAction(..)
  , makeApplication
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
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Lazy.Char8           as LBS8
import qualified Data.HashMap.Strict                  as HashMap
import qualified Data.Map.Strict                      as Map
import           Data.String.Interpolate              ( i )
import qualified Data.Text                            as Text
import           Data.Time.Clock.POSIX                ( POSIXTime, getPOSIXTime )
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
                                                      , WithDynamicContentType(WithDynamicContentType)
                                                      , api
                                                      , downloadGalleryFile
                                                      , fetchBlacklist
                                                      , nextGalleryTask
                                                      , runEHentaiAPI
                                                      , runEHentaiAPIIO
                                                      , startListening
                                                      , stopListening
                                                      )

import           LegacyCiphers

import qualified Mason.Builder                        as BD

import           Middleware

import           Network.HTTP.Client                  ( brReadSome
                                                      , newManager
                                                      , parseRequest
                                                      , withResponse
                                                      )
import           Network.HTTP.Client.TLS              ( tlsManagerSettings )
import           Network.HTTP.Simple                  ( getResponseBody )
import           Network.HTTP.Types                   ( status200, status404 )
import           Network.TLS                          ( Credentials(Credentials), Version (..) )
import           Network.TLS.Extra                    ( ciphersuite_all )
import           Network.Wai                          ( Middleware
                                                      , Request(requestMethod)
                                                      , responseLBS
                                                      )
import           Network.Wai.Handler.Warp             ( defaultSettings, setPort )
import           Network.Wai.Handler.WarpTLS          ( OnInsecure(AllowInsecure)
                                                      , TLSSettings(..)
                                                      , defaultTlsSettings
                                                      , runTLS
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

import           Storage.Database                     ( FileRecord, initializeDB, runCache )
import           Storage.Filesystem                   ( runCacheFilesystem )
import           Storage.Locate
import           Storage.R2                           ( mkR2Connection, runCacheR2 )

import           System.Directory                     ( createDirectoryIfMissing, doesFileExist )
import           System.FilePath                      ( takeDirectory )
import           System.IO                            ( hPutStrLn )
import           System.IO.Unsafe                     ( unsafePerformIO )

import           Types                                ( CacheBackend(..)
                                                      , ClientConfig
                                                      , ClientConfig(..)
                                                      , FileURI(fileExt)
                                                      , GalleryFile(..)
                                                      , GalleryMetadata(..)
                                                      , HathSettings(..)
                                                      , RPCError
                                                      , hentaiHeader
                                                      , parseFileURI
                                                      )

import           UnliftIO                             ( TChan
                                                      , getMonotonicTime
                                                      , newTChanIO
                                                      , race
                                                      , readTChan
                                                      , replicateConcurrently
                                                      , withAsync
                                                      , writeTChan
                                                      )

import           Utils                                ( bufferSending
                                                      , hash
                                                      , log
                                                      , lookupParam
                                                      , parseURLParams
                                                      )

maxTimeDrift :: Int64
maxTimeDrift = 600

{-# NOINLINE globalKeystampMap #-}
globalKeystampMap :: IORef KeystampMap
globalKeystampMap = unsafePerformIO $ newIORef HashMap.empty

{-# NOINLINE globalKeystampMapCounter #-}
globalKeystampMapCounter :: IORef Int
globalKeystampMapCounter = unsafePerformIO $ newIORef 0

maxSameKeystampRequests :: Int
maxSameKeystampRequests = 10

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
      (encodeUtf8 -> filename) = do
      currentTime <- embed getSystemTime
      cfg <- ask @ClientConfig
      when (abs (timestamp - systemSeconds currentTime) > maxTimeDrift)
        $ throw err403 { errBody = "Your time is out of sync. Please update your system time." }
      when (answer /= challange cfg) $ throw err403 { errBody = "Invalid key." }
      isSimpleLimited <- checkKeystampRateLimit
      if isSimpleLimited
        then throw $ err429 { errBody = "Too Many Requests" }
        else locateResource
          LocateURI { locateURIFilename = filename, locateURI = uri, locateURIOptions = opts }
          >>= \case
            Nothing -> throw err404
            Just bs -> do
              incServed
              addUpload (BS.length bs)
              return
                $ addHeader @"Content-Length" (BS.length bs)
                $ WithDynamicContentType mimeType bs
      where
        mimeType = case fileExt uri of
          "jpg" -> "image/jpeg"
          "png" -> "image/png"
          "gif" -> "image/gif"
          "mp4" -> "video/mp4"
          "wbm" -> "video/webm"
          "wbp" -> "video/webp"
          "avf" -> "video/avif"
          "jxl" -> "image/jxl"
          _     -> "application/octet-stream"

        uri = parseFileURI fileid

        ( timestamp :: Int64, answer )
          = let
              rs        = fromMaybe "" (Map.lookup "keystamp" opts)
              ( t, rk ) = BSC.span (/= '-') rs
            in
              ( fromIntegral $ maybe 0 fst $ BSC.readInteger t, BS.tail rk )

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

        -- return True if over limit
        checkKeystampRateLimit = embed @IO $ do
          cnt <- readIORef globalKeystampMapCounter
          if cnt > 100
            then do
              writeIORef globalKeystampMapCounter 0
              writeIORef globalKeystampMap HashMap.empty
              pure False
            else do
              writeIORef globalKeystampMapCounter (cnt + 1)
              now <- getMonotonicTime
              m <- readIORef globalKeystampMap
              let checkKey = fileid <> show timestamp
              case HashMap.lookup checkKey m of
                Nothing -> modifyIORef' globalKeystampMap (HashMap.insert checkKey ( 1, now ))
                  >> pure False
                Just ( times, stamp ) -> if now - stamp > timeWindow
                  then modifyIORef' globalKeystampMap (HashMap.delete checkKey) >> pure False
                  else if times >= maxSameKeystampRequests
                    then pure True
                    else modifyIORef'
                      globalKeystampMap
                      (HashMap.insert checkKey ( times + 1, stamp ))
                      >> pure False

    serverCmdHandler command (parseURLParams . encodeUtf8 -> additional) _time _key
      = case command of
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
                    let ( failed :: Int, millis :: Int64 )
                          = foldl' (\( f, t ) r -> case r of
                                      Nothing -> ( f + 1, t )
                                      Just v  -> ( f, t + v )) ( 0, 0 ) results
                    addUpload (testCount * fromIntegral testSize)
                    plainText [i|OK:#{failed}-#{millis}|]
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
          updateSettings =<< fetchSettings
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
        runTest testSize req mgr = do
          start <- getPOSIXTime
          let phi 0 _  = do
                end <- getPOSIXTime
                return $ truncate $ realToFrac @POSIXTime @Double $ 1000 * (end - start)
              phi n br = do
                s <- LBS.length <$> brReadSome br 1024
                phi (max 0 (n - s)) br
          try @SomeException (withResponse req mgr (phi testSize . getResponseBody)) >>= \case
            Left _  -> return Nothing
            Right v -> return $ Just v

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
      => Sem (KVStore FileURI FileRecord : r) a
      -> Sem r a
  }

-- Create the WAI application with rate limiting
makeApplication :: ServerLoopContext -> Application
makeApplication serverCxt
  = finalMiddleware
  $ normalizeAcceptMiddleware
  $ tracingConnections statsEnv
  $ tracingTimeUsage statsEnv
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
        else rateLimitMiddleware

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

-- | Core server loop implementing the main request-response cycle.
--
-- This function encapsulates the common logic for running the TLS server,
-- handling server actions (reload, cert refresh, shutdown), and coordinating
-- graceful shutdown. The loop is parameterized by a recursion continuation
-- to support different cache backends that may need different state (e.g.,
-- SQLite connection handle).
--
-- The server loop:
-- 1. Creates a WAI application with all configured middleware
-- 2. Notifies the RPC server that we're ready to serve
-- 3. Races between:
--    - Receiving control commands on the action channel
--    - Running the TLS server to handle HTTP requests
-- 4. Handles control commands:
--    - 'GracefulShutdown': Stops listening and exits cleanly
--    - 'Reload': Exits to allow systemd/supervisor to restart
--    - 'Cert': Refreshes TLS certificates and recurses with new certs
--
--
serverLoop :: ServerLoopContext
           -- ^ Server runtime context (config, channels, state, flags, cache)
           -> ( CertificateChain, PrivKey )
           -- ^ Current TLS certificates
           -> IO ( CertificateChain, PrivKey )
serverLoop ctx certs = do
  let ServerLoopContext { loopConfig = cfg
                        , loopChannels = ServerChannels { serverActionChan = chan }
                        , loopState = ServerState { serverSettings = settings }
                        } = ctx

  let app = makeApplication ctx

  st <- readTVarIO settings
  result <- withAsync (notifyStart cfg) $ \_ -> race (atomically $ readTChan chan)
    $ runTLS
      (defaultTlsSettings
       { tlsCredentials = Just (Credentials [ certs ])
       , onInsecure     = AllowInsecure
       , tlsCiphers     = ciphersuite_all
           ++ [ cipher_ECDHE_ECDSA_AES128CBC_SHA
              , cipher_ECDHE_RSA_AES128CBC_SHA
              , cipher_ECDHE_RSA_AES256CBC_SHA
              ]
       , tlsAllowedVersions = [ TLS10, TLS11, TLS12, TLS13 ]
       })
      (setPort (clientPort st) defaultSettings)
    $ logStdout app

  case result of
    Left GracefulShutdown -> gracefulShutdown cfg
    Left Reload -> exitSuccess
    Left Cert -> refreshCerts cfg
    Right _ -> error "Server terminated unexpectedly"

-- | Perform graceful shutdown by notifying RPC server and exiting.
--
-- This function repeatedly calls 'stopListening' until the RPC server
-- acknowledges the shutdown request, then exits the process cleanly.
gracefulShutdown :: ClientConfig -> IO a
gracefulShutdown cfg = go
  where
    go = runEHentaiAPIIO cfg stopListening >>= \case
      Right _ -> exitSuccess
      Left _  -> go

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
        finally (serverLoopMain (ctx cacheRunner) certs) stopTictok
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
              finally (serverLoopMain (ctx cacheRunner) certs) stopTictok
    CacheBackendFilesystem -> do
      let root = Text.unpack $ cachePath config
      -- Ensure cache root exists
      createDirectoryIfMissing True root
      let cacheRunner = CacheRunner { runCacheWith = runCacheFilesystem root }
      stopTictok <- tictok config
      finally (serverLoopMain (ctx cacheRunner) certs) stopTictok
  where
    startsDownloader = do
      metadata <- nextGalleryTask
      incDlTask
      case metadata of
        Nothing -> log Info "No gallery task available"
        Just md -> do
          log Info [i|Starting download for gallery #{galleryID md}|]
          complete <- mapM (verifyAndDownload md) (galleryFileList md)
          when (and complete) (notifyGalleryCompletion md)

    verifyAndDownload md f = do
      let filePath :: FilePath
            = [i|download/#{galleryID md}/#{galleryFileName f}.#{galleryFileExt f}|]
      embed $ createDirectoryIfMissing True (takeDirectory filePath)
      existingBytes <- embed $ do
        existing <- doesFileExist filePath
        if existing
          then BS.readFile filePath
          else return BS.empty
      if galleryFileHash f == hash existingBytes
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
            embed $ BS.writeFile filePath bytes
            pure True

    serverLoopMain :: ServerLoopContext -> ( CertificateChain, PrivKey ) -> IO ()
    serverLoopMain ctx cert = do
      newCert <- serverLoop ctx cert
      serverLoopMain ctx newCert

