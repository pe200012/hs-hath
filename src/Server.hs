{-# LANGUAGE DataKinds #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Server ( startServer, ServerAction(..), makeApplication, CacheRunner(..) ) where

import           Colog                                  ( Message
                                                        , Severity(Info)
                                                        , richMessageAction
                                                        )
import           Colog.Polysemy                         ( Log, runLogAction )

import           Control.Concurrent                     ( forkIO, threadDelay )
import           Control.Concurrent.Suspend             ( mDelay )
import           Control.Concurrent.Timer               ( repeatedTimer, stopTimer )
import           Control.Exception                      ( finally, try )

import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Char8                  as BSC
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.HashMap.Strict                    as HashMap
import qualified Data.Map.Strict                        as Map
import           Data.String.Interpolate                ( i )
import qualified Data.Text                              as Text
import           Data.Time.Clock                        ( NominalDiffTime
                                                        , UTCTime
                                                        , addUTCTime
                                                        , getCurrentTime
                                                        )
import           Data.Time.Clock.POSIX                  ( POSIXTime
                                                        , getPOSIXTime
                                                        , utcTimeToPOSIXSeconds
                                                        )
import           Data.Time.Clock.System                 ( SystemTime(systemSeconds)
                                                        , getSystemTime
                                                        )
import           Data.X509                              ( CertificateChain, PrivKey )

import           Database.SQLite.Simple                 ( withConnection )

import           HathNetwork.Genesis
import           HathNetwork.RPC                        ( RPC
                                                        , notifyGalleryCompletion
                                                        , runRPC
                                                        , runRPCIO
                                                        , stillAlive
                                                        )

import           Interface.API                          ( API
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

import           Network.HTTP.Client                    ( brReadSome
                                                        , newManager
                                                        , parseRequest
                                                        , withResponse
                                                        )
import           Network.HTTP.Client.TLS                ( tlsManagerSettings )
import           Network.HTTP.Simple                    ( getResponseBody )
import           Network.HTTP.Types                     ( hAccept, mkStatus, status200, status404 )
import           Network.Socket                         ( HostAddress, HostAddress6, SockAddr(..) )
import           Network.TLS                            ( Credentials(Credentials) )
import           Network.Wai                            ( Middleware
                                                        , Request(requestMethod)
                                                        , Response
                                                        , rawPathInfo
                                                        , remoteHost
                                                        , requestHeaders
                                                        , responseLBS
                                                        )
import           Network.Wai.Handler.Warp               ( defaultSettings, setPort )
import           Network.Wai.Handler.WarpTLS            ( OnInsecure(AllowInsecure)
                                                        , TLSSettings(..)
                                                        , defaultTlsSettings
                                                        , runTLS
                                                        )
import           Network.Wai.Middleware.RealIp          ( realIpHeader )
import           Network.Wai.Middleware.RequestLogger   ( logStdout, logStdoutDev )

import           Polysemy                               ( Embed
                                                        , Members
                                                        , Sem
                                                        , embed
                                                        , embedToFinal
                                                        , runFinal
                                                        )
import           Polysemy.Async                         ( Async, async, asyncToIOFinal )
import           Polysemy.Error                         ( Error, errorToIOFinal, mapError, throw )
import           Polysemy.KVStore                       ( KVStore )
import           Polysemy.Reader                        ( Reader, ask, runReader )

import           Relude                                 hiding ( Reader, ask, get, runReader )

import           Servant
import           Servant.Client                         ( ClientError )
import qualified Servant.Types.SourceT                  as Source

import           SettingM                               ( SettingM(..)
                                                        , runSettingM
                                                        , updateSettings
                                                        )

import           Stats                                  ( Stats
                                                        , StatsEnv(..)
                                                        , addDlBytes
                                                        , addDownload
                                                        , addUpload
                                                        , incDlFile
                                                        , incDlTask
                                                        , incFetched
                                                        , incServed
                                                        , newStatsEnv
                                                        , readPrometheus
                                                        , runStats
                                                        )

import           Storage.Database                       ( FileRecord, initializeDB, runCache )
import           Storage.Locate
import           Storage.R2                             ( mkR2Connection, runCacheR2 )

import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge

import           Types                                  ( CacheBackend(..)
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

import           UnliftIO                               ( TChan
                                                        , race
                                                        , readTChan
                                                        , replicateConcurrently
                                                        , withAsync
                                                        , writeTChan
                                                        )

import           Utils                                  ( bufferSending
                                                        , hash
                                                        , log
                                                        , lookupParam
                                                        , parseURLParams
                                                        )

maxTimeDrift :: Int64
maxTimeDrift = 600

timeWindow :: NominalDiffTime
timeWindow = 10

maxRequests :: Int
maxRequests = 5

data ServerAction = Reload | Cert | GracefulShutdown

-- Data types for tracking requests
-- Note: Lists cannot be unpacked, so we use strict spine with explicit strictness
data IPRecord
  = IPRecord { requestTimes :: ![ UTCTime ]       -- ^ Times of recent requests (strict spine)
             , bannedUntil  :: !(Maybe UTCTime) -- ^ When the ban expires
             }
  deriving ( Show )

-- Change the Map key from SockAddr to a custom IP type
data IP = IPv4 {-# UNPACK #-} !HostAddress | IPv6 {-# UNPACK #-} !HostAddress6
  deriving ( Eq, Ord, Show, Generic )

instance Hashable IP

type IPMap = HashMap IP IPRecord

{-# INLINE getIP #-}
-- Helper function to extract just the IP from a SockAddr
getIP :: SockAddr -> Maybe IP
getIP (SockAddrInet _ ha) = Just $ IPv4 ha
getIP (SockAddrInet6 _ _ ha _) = Just $ IPv6 ha
getIP _ = Nothing

-- Create a new rate limiting middleware
rateLimitMiddleware :: TVar IPMap -> Middleware
rateLimitMiddleware ipMap app req k
  | "/h/" `BS.isPrefixOf` rawPathInfo req = do
    now <- getCurrentTime
    let sockAddr = remoteHost req
    case getIP sockAddr of
      Nothing -> app req k  -- If we can't get IP, just allow the request
      Just ip -> do
        allowed <- checkRateLimit ipMap ip now
        if allowed
          then app req k
          else k tooManyRequestsResponse
  | otherwise = app req k  -- Skip rate limiting for non-resource paths

-- Check if request is allowed and update rate limit state
checkRateLimit :: TVar IPMap -> IP -> UTCTime -> IO Bool
checkRateLimit ipMap ip now = atomically $ do
  m <- readTVar ipMap
  maybe firstRequest existingRequest (HashMap.lookup ip m)
  where
    -- Handle first request from an IP by creating new record
    firstRequest = do
      modifyTVar' ipMap $ HashMap.insert ip (IPRecord [ now ] Nothing)
      return True

    -- Check if IP is banned, otherwise check request count
    existingRequest record = case bannedUntil record of
      Just banTime
        | banTime > now -> return False
      _ -> checkAndUpdateRequests record

    -- Check if requests are within rate limit window and update accordingly
    checkAndUpdateRequests record = do
      let windowStart = addUTCTime (-timeWindow) now
          -- Use filter and force strict evaluation to avoid space leaks
          -- The list spine and length are evaluated immediately, preventing thunk accumulation
          !recentRequests = filter (> windowStart) (requestTimes record)
          !len = length recentRequests
          !newRequests = now : recentRequests

      if len >= maxRequests
        then banIP newRequests
        else allowRequest newRequests

    -- Ban IP for 10 minutes if too many requests
    banIP newRequests = do
      let banUntil = addUTCTime (10 * 60) now  -- 10 minute ban
      modifyTVar' ipMap $ HashMap.insert ip (IPRecord newRequests (Just banUntil))
      return False

    -- Allow request and update record with new request time
    allowRequest newRequests = do
      modifyTVar' ipMap $ HashMap.insert ip (IPRecord newRequests Nothing)
      return True

{-# INLINE tooManyRequestsResponse #-}
-- Standard 429 response
tooManyRequestsResponse :: Response
tooManyRequestsResponse
  = responseLBS
    (mkStatus 429 "Too Many Requests")
    [ ( "Content-Type", "text/plain" ) ]
    "Too Many Requests"

type KeystampMap = HashMap Int64 ( Int, UTCTime )

-- | Keystamp rate limiting middleware
keystampLimitMiddleware :: TVar KeystampMap -> Middleware
keystampLimitMiddleware ksVar app req sendResponse
  | "/h/" `BS.isPrefixOf` path = do
    let parts = BS.split 47 (BS.drop 1 path) -- 47 is '/'
    case parts of
      (_h : _fileid : optsRaw : _) -> do
        let opts = parseURLParams optsRaw
        case Map.lookup "keystamp" opts of
          Just ksRaw -> do
            let ( t, _ ) = BSC.span (/= '-') ksRaw
            case BSC.readInteger t of
              Just ( fromIntegral -> ts, _ ) -> do
                now <- getCurrentTime
                allowed <- atomically $ do
                  m <- readTVar ksVar
                  let ( count, _ ) = HashMap.lookupDefault ( 0, now ) ts m
                  if count >= 10  -- Undocumented limit
                    then return False
                    else do
                      let !newM = HashMap.insert ts ( count + 1, now ) m
                      writeTVar ksVar newM
                      return True
                if allowed
                  then app req sendResponse
                  else sendResponse tooManyRequestsResponse
              Nothing -> app req sendResponse
          Nothing    -> app req sendResponse
      _ -> app req sendResponse
  | otherwise = app req sendResponse
  where
    path = rawPathInfo req

-- | Track active connections
tracingConnections :: StatsEnv -> Middleware
tracingConnections statsEnv app req k = do
  Gauge.inc (statsActiveConnectionsGauge statsEnv)
  finally (app req k) (Gauge.dec (statsActiveConnectionsGauge statsEnv))

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
      res <- locateResource
        LocateURI { locateURIFilename = filename, locateURI = uri, locateURIOptions = opts }
      case res of
        Nothing -> throw err404
        Just bs -> do
          incServed
          addUpload (BS.length bs)
          return $ addHeader @"Content-Length" (BS.length bs) $ WithDynamicContentType mimeType bs
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
        challange :: ClientConfig -> ByteString
        challange cfg = BS.take 10 (hash [i|#{timestamp}-#{fileid}-#{key cfg}-hotlinkthis|])

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
                , [i|#{protocol}://#{hostname}:#{port}/t/#{testSize}/#{testTime}/#{testKey}/0|]
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
          void $ async startsDownloader
          plainText ""
        RefreshCerts      -> do
          log Info "Refreshing certificates"
          chan <- ask @(TChan ServerAction)
          void $ embed $ forkIO $ do
            threadDelay (1000000 * 2)
            atomically $ writeTChan chan Cert
          plainText ""
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
                = [i|download/#{galleryTitle md}/#{galleryFileName f}.#{galleryFileExt f}|]
          existingBytes <- embed $ BS.readFile filePath
          if galleryFileHash f == hash existingBytes
            then log Info [i|#{galleryFileName f}.#{galleryFileExt f} already verified, skipping|]
              >> pure True
            else downloadGalleryFile md f >>= \case
              Nothing    -> pure False
              Just bytes -> do
                log Info [i|Downloaded #{galleryFileName f}.#{galleryFileExt f}|]
                incDlFile
                addDlBytes (BS.length bytes)
                embed $ BS.writeFile filePath bytes
                pure True

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
        challange :: ClientConfig -> ByteString
        challange cfg
          = hash
            @ByteString
            [i|hentai@home-speedtest-#{testSize}-#{testTime}-#{clientId cfg}-#{key cfg}|]

    rawHandler req k = case requestMethod req of
      -- Handle HEAD requests with 200 OK and empty body
      "HEAD" -> k $ responseLBS status200 hentaiHeader ""

      _      -> k $ responseLBS status404 [] ""

-- | Set up periodic timers for various housekeeping tasks
tictok :: ClientConfig -> TVar IPMap -> TVar KeystampMap -> IO (IO ())
tictok config ipMap ksVar = do
  -- ipmap cleanup
  cleanupHandle <- repeatedTimer
    (do
       now <- getCurrentTime
       atomically $ do
         m <- readTVar ipMap
         let windowStart = addUTCTime (-timeWindow) now
         let filtered = HashMap.filter (\record -> case bannedUntil record of
                                          Just banTime
                                            | banTime > now -> True
                                          _ -> case requestTimes record of
                                            (t : _) -> t > windowStart
                                            []      -> False) m
         writeTVar ipMap filtered)
    (mDelay 1)

  -- keystamp cleanup (every minute, remove entries older than 2 hours)
  ksCleanupHandle <- repeatedTimer
    (do
       now <- getCurrentTime
       atomically $ do
         m <- readTVar ksVar
         -- Keep entries where (timestamp > now - 30 mins)
         -- Since keystamp is a unix timestamp (seconds), we compare it directly
         let twoHoursAgo
               = truncate (realToFrac (utcTimeToPOSIXSeconds (addUTCTime (-1800) now)) :: Double)
                 :: Int64
         let filtered = HashMap.filterWithKey (\ts _ -> ts > twoHoursAgo) m
         writeTVar ksVar filtered)
    (mDelay 60)

  -- heartbeat
  heartbeatHandle <- repeatedTimer (void $ runRPCIO config stillAlive) (mDelay 1)

  -- resource blacklist
  blacklistHandle <- repeatedTimer (do
                                      res <- runRPCIO config (fetchBlacklist 43200)
                                      print res) (mDelay 36)

  return $ do
    stopTimer cleanupHandle
    stopTimer ksCleanupHandle
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
  = CacheRunner { runCacheWith :: forall r a. Members '[ Embed IO, Log Message, Error RPCError ] r
                               => Sem (KVStore FileURI FileRecord : r) a
                               -> Sem r a
                }

-- Create the WAI application with rate limiting
makeApplication
  :: ClientConfig
  -> TVar HathSettings
  -> TChan ServerAction
  -> TVar IPMap
  -> TVar KeystampMap
  -> Bool  -- ^ Disable rate limiting
  -> Bool  -- ^ Trust proxy headers for real IP
  -> CacheRunner
  -> StatsEnv
  -> Application
makeApplication
  config
  settings
  action
  ipMap
  ksVar
  disableRateLimit
  trustProxyHeaders
  cacheRunner
  statsEnv
  = finalMiddleware
  $ normalizeAcceptMiddleware
  $ tracingConnections statsEnv
  $ serve api (hoistServer api interpretServer server)
  where
    applyRealIp :: Middleware
    applyRealIp
      = if trustProxyHeaders
        then realIpHeader "X-Forwarded-For"
        else id

    applyRateLimit :: Middleware
    applyRateLimit
      = if disableRateLimit
        then id
        else rateLimitMiddleware ipMap

    applyKeystampLimit :: Middleware
    applyKeystampLimit = keystampLimitMiddleware ksVar

    finalMiddleware :: Middleware
    finalMiddleware = applyRealIp . applyKeystampLimit . applyRateLimit

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
      . runReader config
      . runReader action
      . runReader statsEnv
      . runLogAction @IO @Message richMessageAction
      . runEHentaiAPI
      . runCacheWith cacheRunner
      . runStats
      . runLocate
      . runGenesis
      . runRPC

startServer :: ClientConfig
            -> HathSettings
            -> ( CertificateChain, PrivKey )
            -> TChan ServerAction
            -> Bool
            -> Bool
            -> IO ()
startServer config settings certs chan disableRateLimit trustProxyHeaders = do
  ipMap <- newTVarIO HashMap.empty
  ksVar <- newTVarIO HashMap.empty
  ssVar <- newTVarIO settings
  statsEnv <- newStatsEnv
  print =<< runRPCIO config (fetchBlacklist 259200)
  case cacheBackend config of
    CacheBackendSQLite -> do
      withConnection (Text.unpack $ cachePath config) $ \conn -> do
        initializeDB conn
        let cacheRunner = CacheRunner { runCacheWith = runCache conn }
        stopTictok <- tictok config ipMap ksVar
        finally
          (loopSQLite
             config
             ssVar
             certs
             conn
             cacheRunner
             ipMap
             ksVar
             disableRateLimit
             trustProxyHeaders
             statsEnv)
          stopTictok
    CacheBackendR2     -> do
      case r2Config config of
        Nothing    -> error "cacheBackend=r2 but r2Config is missing"
        Just r2Cfg -> do
          r2ConnResult <- mkR2Connection r2Cfg
          case r2ConnResult of
            Left err     -> error $ "Failed to initialize R2: " <> err
            Right r2Conn -> do
              let cacheRunner = CacheRunner { runCacheWith = runCacheR2 r2Conn }
              stopTictok <- tictok config ipMap ksVar
              finally
                (loopR2
                   config
                   ssVar
                   certs
                   cacheRunner
                   ipMap
                   ksVar
                   disableRateLimit
                   trustProxyHeaders
                   statsEnv)
                stopTictok
  where
    gracefulShutdown :: IO a
    gracefulShutdown
      = let
          phi = flip unless phi . fromRight False =<< runEHentaiAPIIO config stopListening
        in 
          phi >> exitSuccess

    refreshCerts = phi 0
      where
        phi (retries :: Int) = runGenesisIO config fetchCertificate >>= \case
          Right (Right newCerts) -> return newCerts
          e -> do
            putStrLn $ "Failed to refresh certs: " <> show e
            if retries > 3
              then do
                putStrLn "Giving up"
                gracefulShutdown
              else do
                putStrLn $ "Retrying in " <> show (2 ^ retries :: Int) <> " seconds"
                threadDelay (1000000 * 2 ^ retries)
                phi (retries + 1)

    -- SQLite loop - needs Connection for tictok
    loopSQLite cfg set c conn cacheRunner ipMap ksVar disableRL trustProxy statsEnv = do
      let app = makeApplication cfg set chan ipMap ksVar disableRL trustProxy cacheRunner statsEnv
      st <- readTVarIO set
      result <- withAsync (notifyStart cfg) $ \_ -> race (atomically $ readTChan chan)
        $ runTLS
          (defaultTlsSettings
           { tlsCredentials = Just (Credentials [ c ]), onInsecure = AllowInsecure })
          (setPort (clientPort st) defaultSettings)
        $ logStdoutDev
        $ logStdout app
      case result of
        Left GracefulShutdown -> gracefulShutdown
        Left Reload -> exitSuccess
        Left Cert -> do
          newCerts <- refreshCerts
          loopSQLite cfg set newCerts conn cacheRunner ipMap ksVar disableRL trustProxy statsEnv
        Right _ -> error "Server terminated unexpectedly"

    -- R2 loop - no Connection needed
    loopR2 cfg set c cacheRunner ipMap ksVar disableRL trustProxy statsEnv = do
      let app = makeApplication cfg set chan ipMap ksVar disableRL trustProxy cacheRunner statsEnv
      st <- readTVarIO set
      result <- withAsync (notifyStart cfg) $ \_ -> race (atomically $ readTChan chan)
        $ runTLS
          (defaultTlsSettings
           { tlsCredentials = Just (Credentials [ c ]), onInsecure = AllowInsecure })
          (setPort (clientPort st) defaultSettings)
        $ logStdoutDev
        $ logStdout app
      case result of
        Left GracefulShutdown -> gracefulShutdown
        Left Reload -> exitSuccess
        Left Cert -> do
          newCerts <- refreshCerts
          loopR2 cfg set newCerts cacheRunner ipMap ksVar disableRL trustProxy statsEnv
        Right _ -> error "Server terminated unexpectedly"

normalizeAcceptMiddleware :: Middleware
normalizeAcceptMiddleware app req = app req { requestHeaders = normalizedHeaders }
  where
    normalizedHeaders = map normalizeHeader (requestHeaders req)

    normalizeHeader ( name, value )
      | name == hAccept = ( name, "*/*" )
      | otherwise = ( name, value )

