-- | The web server: the application it serves, and the client's life around
-- it, from client_start to client_stop.
module Hath.Server
  ( startServer
  , ServerOptions(..)
  , makeApplication
  , ServerEnv(..)
  , ServerAction(..)
  , needsRenewal
  ) where

import           Control.Concurrent                   ( threadDelay )

import           Data.Hourglass                       ( Elapsed(..), Seconds(..), timeGetElapsed )
import           Data.String.Interpolate              ( i )
import           Data.X509                            ( CertificateChain(..)
                                                      , PrivKey
                                                      , certValidity
                                                      )
import qualified Data.X509                            as X509

import           Database.SQLite.Simple               ( withConnection )

import           Hath.Cache                           ( CacheRunner(..), deleteFile )
import           Hath.Cache.Filesystem                ( newFsCache
                                                      , runCacheFilesystem
                                                      , runEvictor
                                                      )
import           Hath.Cache.R2                        ( mkR2Connection, runCacheR2 )
import           Hath.Cache.SQLite                    ( initializeDB, runCacheSQLite )
import           Hath.Config                          ( CacheBackend(..), ClientConfig(..) )
import           Hath.Error                           ( HathError )
import           Hath.Gallery.Downloader              ( runGalleryDownloader )
import           Hath.Log                             ( logWarning )
import           Hath.RPC                             ( StartFailure(..)
                                                      , clientSettings
                                                      , clientStart
                                                      , clientStop
                                                      , getBlacklist
                                                      , getCertificate
                                                      , overload
                                                      , runRPCIO
                                                      , startFailure
                                                      , stillAlive
                                                      , toldToStop
                                                      )
import           Hath.Server.App                      ( ServerAction(..), ServerEnv(..) )
import           Hath.Server.ConnectionLimit          ( connectionLimit )
import           Hath.Server.FloodControl             ( isRPCServer, newFloodControl )
import           Hath.Server.Handlers                 ( application )
import           Hath.Server.LegacyCiphers
import           Hath.Server.Middleware
import           Hath.ServerTime                      ( serverNow )
import           Hath.Settings                        ( HathSettings(..) )
import           Hath.Stats                           ( newStatsEnv )

import           Network.TLS                          ( Credentials(Credentials)
                                                      , TLSException(HandshakeFailed)
                                                      , Version(..)
                                                      )
import           Network.TLS.Extra                    ( ciphersuite_all )
import qualified Network.TLS.SessionManager           as SM
import           Network.Wai                          ( Application, Request )
import           Network.Wai.Handler.Warp             ( defaultOnException
                                                      , defaultSettings
                                                      , setBeforeMainLoop
                                                      , setGracefulShutdownTimeout
                                                      , setInstallShutdownHandler
                                                      , setOnClose
                                                      , setOnException
                                                      , setOnOpen
                                                      , setPort
                                                      )
import           Network.Wai.Handler.WarpTLS          ( OnInsecure(AllowInsecure)
                                                      , TLSSettings(..)
                                                      , WarpTLSException(ClientClosedConnectionPrematurely)
                                                      , runTLS
                                                      , tlsSettingsSni
                                                      )
import           Network.Wai.Middleware.RealIp        ( realIpHeader )
import           Network.Wai.Middleware.RequestLogger ( logStdout )

import           Polysemy.Error                       ( catch )

import           Relude

import           UnliftIO                             ( TChan
                                                      , mapConcurrently_
                                                      , newTChanIO
                                                      , race
                                                      , readTChan
                                                      , tryAny
                                                      , waitCatch
                                                      , withAsync
                                                      , writeTChan
                                                      )

-- | The WAI application: the 'application' behind its middleware.
makeApplication :: ServerEnv -> Application
makeApplication env = middleware (application env)
  where
    middleware = realIp . floodControl . tracingConnections stats . tracingTimeUsage stats

    stats = envStats env

    realIp
      | envTrustProxyHeaders env = realIpHeader "X-Forwarded-For"
      | otherwise = id

    floodControl = maybe id (`rateLimitMiddleware` fromRPCServer) (envFloodControl env)

    fromRPCServer ip = (`isRPCServer` ip) . rpcServerIPs <$> readTVarIO (envSettings env)

data ServerOptions
  = ServerOptions { disableRateLimit  :: !Bool
                    -- ^ no flood control, for when every reader appears to come from one address
                  , trustProxyHeaders :: !Bool
                    -- ^ take the reader's address from @X-Forwarded-For@
                  }

-- | Serve until told to shut down, then return; exit if the RPC server will not
-- have us. The settings and certificate are the ones it gave us at login.
startServer :: ClientConfig
            -> ServerOptions
            -> HathSettings
            -> ( CertificateChain, PrivKey )
            -> TChan ServerAction
            -> IO ()
startServer config options settings certs actions = do
  settingsVar <- newTVarIO settings
  galleryTasks <- newTChanIO
  stats <- newStatsEnv
  floodControl <- if disableRateLimit options
    then pure Nothing
    else Just <$> newFloodControl
  runGalleryDownloader config stats galleryTasks
    `alongside` withCacheBackend
      config
      (diskLimitBytes <$> readTVarIO settingsVar)
      (\cache -> serverLoop
         ServerEnv { envConfig = config
                   , envSettings = settingsVar
                   , envActions = actions
                   , envGalleryTasks = galleryTasks
                   , envStats = stats
                   , envCache = cache
                   , envFloodControl = floodControl
                   , envTrustProxyHeaders = trustProxyHeaders options
                   }
         certs)

-- | Run the second action with the first running beside it. The first is
-- stopped when the second ends.
alongside :: IO a -> IO b -> IO b
alongside background action = withAsync background (const action)

-- | Open the cache the config asks for. The Filesystem backend also gets its
-- evictor, which follows the disk limit the server assigns.
withCacheBackend :: ClientConfig -> IO Int64 -> (CacheRunner -> IO a) -> IO a
withCacheBackend config diskLimit use = case cacheBackend config of
  CacheBackendSQLite     -> withConnection (toString $ cachePath config) $ \conn -> do
    initializeDB conn
    use CacheRunner { runCacheWith = runCacheSQLite conn }
  CacheBackendR2         -> case r2Config config of
    Nothing -> die "cacheBackend=r2 but r2Config is missing"
    Just r2 -> mkR2Connection r2 >>= \case
      Left err   -> die $ "Failed to initialize R2: " <> toString err
      Right conn -> use CacheRunner { runCacheWith = runCacheR2 conn }
  CacheBackendFilesystem -> do
    fs <- newFsCache (toString $ cachePath config)
    runEvictor fs diskLimit `alongside` use CacheRunner { runCacheWith = runCacheFilesystem fs }

-- | Bind the port, announce ourselves with client_start, then serve and
-- handle commands until one ends the client (official HentaiAtHomeClient.run):
--
-- * 'Cert' fetches a new certificate and swaps it in for the next handshakes;
--   the listener and open transfers carry on.
-- * 'GracefulShutdown' ends the client as 'shutDown' says.
serverLoop :: ServerEnv -> ( CertificateChain, PrivKey ) -> IO ()
serverLoop env certs = do
  let config = envConfig env
  credentials <- newIORef certs
  port <- clientPort <$> readTVarIO (envSettings env)
  listening <- newEmptyMVar
  closeListener <- newEmptyMVar
  ( admit, release ) <- connectionLimit (envSettings env) (notifyOverload config)
  let warpSettings
        = setBeforeMainLoop (putMVar listening ())
        $ setInstallShutdownHandler (putMVar closeListener)
        $ setOnOpen admit
        $ setOnClose release
        $ setGracefulShutdownTimeout (Just 25)
        $ setOnException quietHandshakes
        $ setPort port defaultSettings
  withAsync (runTLS (tlsSettings credentials) warpSettings $ logStdout (makeApplication env))
    $ \web -> do
      -- the server tests our port before it answers client_start
      race (waitCatch web) (takeMVar listening) >>= \case
        Left outcome -> die [i|Could not listen on port #{port}: #{outcome}|]
        Right ()     -> pass
      started <- announce config port
      let untilShutdown
            | started = housekeeping env credentials `alongside` commands env credentials
            | otherwise = commands env credentials
      race (waitCatch web) untilShutdown >>= \case
        Left outcome -> die [i|The web server stopped: #{outcome}|]
        Right ()
          | started -> shutDown config (join $ readMVar closeListener) (void $ waitCatch web)
          | otherwise -> pass
  where
    tlsSettings credentials
      -- asked on every handshake (SNI or not), so a swapped certificate applies at once
      = (tlsSettingsSni $ \_ -> Credentials . pure <$> readIORef credentials)
        { onInsecure = AllowInsecure
        , tlsCiphers = ciphersuite_all
            ++ [ cipher_ECDHE_ECDSA_AES128CBC_SHA
               , cipher_ECDHE_RSA_AES128CBC_SHA
               , cipher_ECDHE_RSA_AES256CBC_SHA
               ]
          -- tls >= 2 speaks nothing older; it rejects SSL3/TLS1.0/TLS1.1 hellos before looking at this
        , tlsAllowedVersions = [ TLS12, TLS13 ]
          -- resumed handshakes skip the certificate and its RSA signature (several ms on a small VM)
        , tlsSessionManagerConfig = Just SM.defaultConfig
        }

-- | Warp's exception printer, minus connections that never got past TLS:
-- scanners probing old protocols and cipher suites, and clients that connect
-- and send nothing (thousands a day). None of them reached the application.
quietHandshakes :: Maybe Request -> SomeException -> IO ()
quietHandshakes req e
  | Just (HandshakeFailed _) <- fromException e = pass
  | Just ClientClosedConnectionPrematurely <- fromException e = pass
  | otherwise = defaultOnException req e

-- | Tell the RPC server we are near our connection limit.
notifyOverload :: ClientConfig -> IO ()
notifyOverload config = runRPCIO config overload >>= \case
  Left failure -> putStrLn [i|Overload notification failed: #{failure}|]
  Right ()     -> pass

-- | Send client_start, once. True if the server took us; if it refuses us for
-- good, exit.
announce :: ClientConfig -> Int -> IO Bool
announce config port = runRPCIO config clientStart >>= \case
  Right ()     -> True <$ putStrLn "Start notification successful."
  Left failure -> case startFailure port failure of
    Refused why     -> die (toString why)
    Unannounced why -> False <$ putStrLn (toString why)

-- | Handle commands until one ends the client.
commands :: ServerEnv -> IORef ( CertificateChain, PrivKey ) -> IO ()
commands env credentials = atomically (readTChan (envActions env)) >>= \case
  GracefulShutdown -> pass
  Cert -> do
    renewed <- refreshCerts (envConfig env)
    now <- serverNow
    case renewed of
      Just new@( chain, _ )
        | not (needsRenewal now chain) -> writeIORef credentials new >> commands env credentials
        -- the official client refuses such a certificate too
        | otherwise -> putStrLn "The new certificate expires within a day as well; shutting down"
      Nothing -> putStrLn "Could not get a new certificate; shutting down"

-- | What the official client does once client_start went through: refresh the
-- settings, delete the files blacklisted in the last three days, then, at its
-- intervals, send still_alive (110 s), delete what the last 12 hours
-- blacklisted (6 h) and look at the certificate (5 min). What ends the client
-- goes through the command channel.
housekeeping :: ServerEnv -> IORef ( CertificateChain, PrivKey ) -> IO ()
housekeeping env credentials = do
  logFailure refreshSettings
  logFailure $ applyBlacklist config (envCache env) 259200
  mapConcurrently_
    (\( delay, task ) -> infinitely $ threadDelay delay >> logFailure task)
    [ ( seconds 110, heartbeat )
    , ( hours 6, applyBlacklist config (envCache env) 43200 )
    , ( minutes 5, checkCertificate )
    ]
  where
    config = envConfig env

    refreshSettings = readTVarIO (envSettings env) >>= runRPCIO config . clientSettings >>= \case
      Right settings -> atomically $ writeTVar (envSettings env) settings
      Left failure   -> putStrLn [i|Failed to refresh settings: #{failure}|]

    heartbeat = runRPCIO config stillAlive >>= \case
      Right ()     -> pass
      Left failure
        | toldToStop failure -> do
          putStrLn
            "The client is shutting down since the network is misconfigured; correct firewall/forwarding settings then restart the client."
          send GracefulShutdown
        | otherwise -> putStrLn [i|Failed stillAlive test: #{failure}; will retry later|]

    -- the official client gives up a day before the certificate expires; we fetch another first
    checkCertificate = do
      now <- serverNow
      ( chain, _ ) <- readIORef credentials
      when (needsRenewal now chain) $ do
        putStrLn "The certificate expires within a day; fetching a new one"
        send Cert

    send = atomically . writeTChan (envActions env)

    -- an exception one task did not expect must not stop the heartbeat
    logFailure task = tryAny task >>= \case
      Left e   -> putStrLn [i|Housekeeping task failed: #{e}|]
      Right () -> pass

    seconds n = n * 1000000

    minutes n = seconds (n * 60)

    hours n = minutes (n * 60)

-- | Delete the files blacklisted in the last so many seconds (official
-- CacheHandler.processBlacklist). The list can be long: only its length is logged.
applyBlacklist :: ClientConfig -> CacheRunner -> Int -> IO ()
applyBlacklist config cache seconds = do
  outcome <- runRPCIO config $ do
    files <- getBlacklist seconds
    -- one file that cannot be deleted must not keep the others
    runCacheWith cache $ for_ files $ \uri -> deleteFile uri `catch` \(failure :: HathError)
      -> logWarning [i|Could not delete blacklisted file #{uri}: #{failure}|]
    pure (length files)
  putStrLn $ case outcome of
    Left failure -> [i|Failed to retrieve the file blacklist, will try again later: #{failure}|]
    Right count  -> [i|Blacklist: #{count} files, deleted from the cache where present|]

-- | End a client that client_start announced (official
-- HentaiAtHomeClient.shutdown and httpServerShutdown): client_stop, so the
-- network sends no one new; 5 s more of serving, for readers already on their
-- way; then no new connections, and up to 25 s for open ones to finish.
shutDown :: ClientConfig -> IO () -> IO () -> IO ()
shutDown config closeListener drained = do
  runRPCIO config clientStop >>= \case
    Left failure -> putStrLn [i|Shutdown notification failed: #{failure}|]
    Right ()     -> pass
  putStrLn "Shutdown in progress - please wait up to 30 seconds"
  threadDelay 5000000
  closeListener
  drained

-- | Whether a certificate must be replaced at this Unix time: with less than a
-- day left, the official client will not serve it (HTTPServer.isCertExpired).
needsRenewal :: Int64 -> CertificateChain -> Bool
needsRenewal now = \case
  CertificateChain (leaf : _) -> notAfter leaf < now + 86400
  CertificateChain []         -> True
  where
    notAfter leaf = case timeGetElapsed (snd $ certValidity $ X509.getCertificate leaf) of
      Elapsed (Seconds unixTime) -> unixTime

-- | Fetch a new certificate, retrying three times with exponential backoff
-- (1, 2, 4 seconds). Nothing if none comes.
refreshCerts :: ClientConfig -> IO (Maybe ( CertificateChain, PrivKey ))
refreshCerts config = go (0 :: Int)
  where
    go retries = runRPCIO config getCertificate >>= \case
      Right newCerts -> pure (Just newCerts)
      Left e
        | retries >= 3 -> do
          putStrLn $ "Failed to refresh certs: " <> show e
          putStrLn "Giving up after 3 retries"
          pure Nothing
        | otherwise -> do
          let delaySeconds = 2 ^ retries :: Int
          putStrLn $ "Failed to refresh certs: " <> show e
          putStrLn
            $ "Retrying in "
            <> show delaySeconds
            <> " seconds (attempt "
            <> show (retries + 1)
            <> "/3)"
          threadDelay (1000000 * delaySeconds)
          go (retries + 1)
