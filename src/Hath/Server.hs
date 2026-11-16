-- | The web server: the application it serves and the loop that keeps it running.
module Hath.Server
  ( startServer
  , ServerOptions(..)
  , makeApplication
  , ServerEnv(..)
  , ServerAction(..)
  , CacheRunner(..)
  , GalleryTask(..)
  ) where

import           Control.Concurrent                   ( forkIO, threadDelay )

import           Data.X509                            ( CertificateChain, PrivKey )

import           Database.SQLite.Simple               ( withConnection )

import           Hath.Cache                           ( CacheRunner(..) )
import           Hath.Cache.Filesystem                ( newFsCache, runCacheFilesystem, runEvictor )
import           Hath.Cache.R2                        ( mkR2Connection, runCacheR2 )
import           Hath.Cache.SQLite                    ( initializeDB, runCacheSQLite )
import           Hath.Config                          ( CacheBackend(..), ClientConfig(..) )
import           Hath.Gallery.Downloader              ( GalleryTask(..), runGalleryDownloader )
import           Hath.RPC                             ( clientStart
                                                      , clientStop
                                                      , getBlacklist
                                                      , getCertificate
                                                      , runRPCIO
                                                      , stillAlive
                                                      )
import           Hath.Server.API                      ( api )
import           Hath.Server.App                      ( ServerAction(..), ServerEnv(..), runApp )
import           Hath.Server.FloodControl             ( newFloodControl, parseIPv4 )
import           Hath.Server.Handlers                 ( server )
import           Hath.Server.LegacyCiphers
import           Hath.Server.Middleware
import           Hath.Settings                        ( HathSettings(..) )
import           Hath.Stats                           ( newStatsEnv )

import           Network.TLS                          ( Credentials(Credentials)
                                                      , TLSException(HandshakeFailed)
                                                      , Version(..)
                                                      )
import           Network.TLS.Extra                    ( ciphersuite_all )
import qualified Network.TLS.SessionManager           as SM
import           Network.Wai                          ( Request )
import           Network.Wai.Handler.Warp             ( defaultOnException
                                                      , defaultSettings
                                                      , setOnException
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

import           Relude

import           Servant                              ( Application, hoistServer, serve )

import           UnliftIO                             ( TChan, newTChanIO, race, readTChan, withAsync )

-- | The WAI application: the 'server' behind its middleware.
makeApplication :: ServerEnv -> Application
makeApplication env
  = realIp
  $ floodControl
  $ normalizeAcceptMiddleware
  $ tracingConnections stats
  $ tracingTimeUsage stats
  $ withHathHeaders
  $ serve api (hoistServer api (runApp env) server)
  where
    stats = envStats env

    realIp
      | envTrustProxyHeaders env = realIpHeader "X-Forwarded-For"
      | otherwise = id

    floodControl = maybe id (`rateLimitMiddleware` isRpcServer) (envFloodControl env)

    -- dual-stack sockets would show RPC servers as ::ffff:a.b.c.d; Warp listens on IPv4 here
    isRpcServer ip = elem ip . mapMaybe parseIPv4 . rpcServerIPs <$> readTVarIO (envSettings env)

data ServerOptions
  = ServerOptions
  { disableRateLimit  :: !Bool
    -- ^ no flood control, for when every reader appears to come from one address
  , trustProxyHeaders :: !Bool
    -- ^ take the reader's address from @X-Forwarded-For@
  }

-- | Serve until told to shut down. The settings and certificate are the ones
-- the RPC server gave us at login.
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
  void $ forkIO $ runGalleryDownloader config stats galleryTasks
  print =<< runRPCIO config (getBlacklist 259200)
  withCacheBackend config (diskLimitBytes <$> readTVarIO settingsVar)
    $ \cache -> withHousekeeping config
    $ serverLoop
      ServerEnv
      { envConfig            = config
      , envSettings          = settingsVar
      , envActions           = actions
      , envGalleryTasks      = galleryTasks
      , envStats             = stats
      , envCache             = cache
      , envFloodControl      = floodControl
      , envTrustProxyHeaders = trustProxyHeaders options
      }
      certs

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
    withAsync (runEvictor fs diskLimit) $ \_ -> use CacheRunner { runCacheWith = runCacheFilesystem fs }

-- | Run an action while a heartbeat goes to the RPC server every minute and
-- the blacklist is fetched every 36 minutes.
withHousekeeping :: ClientConfig -> IO a -> IO a
withHousekeeping config action
  = withAsync (every (minutes 1) $ void $ runRPCIO config stillAlive)
  $ \_ -> withAsync (every (minutes 36) $ print =<< runRPCIO config (getBlacklist 43200))
  $ \_ -> action
  where
    every delay task = forever $ threadDelay delay >> task

    minutes n = n * 60 * 1000000

-- | Tell the RPC server we are listening, then handle commands while the TLS
-- server runs:
--
-- * 'Cert' fetches a new certificate and swaps it in for the next handshakes;
--   the listener and open transfers carry on.
-- * 'GracefulShutdown' tells the RPC server we stop, then exits.
serverLoop :: ServerEnv -> ( CertificateChain, PrivKey ) -> IO ()
serverLoop env certs = do
  let config = envConfig env
  credentials <- newIORef certs
  port <- clientPort <$> readTVarIO (envSettings env)
  let untilShutdown = atomically (readTChan (envActions env)) >>= \case
        Cert -> refreshCerts config >>= writeIORef credentials >> untilShutdown
        GracefulShutdown -> pass
  outcome <- withAsync (notifyStart config) $ \_ -> race untilShutdown
    $ runTLS
      -- asked on every handshake (SNI or not), so a swapped certificate applies at once
      ((tlsSettingsSni $ \_ -> Credentials . pure <$> readIORef credentials)
       { onInsecure         = AllowInsecure
       , tlsCiphers         = ciphersuite_all
           ++ [ cipher_ECDHE_ECDSA_AES128CBC_SHA
              , cipher_ECDHE_RSA_AES128CBC_SHA
              , cipher_ECDHE_RSA_AES256CBC_SHA
              ]
         -- tls >= 2 speaks nothing older; it rejects SSL3/TLS1.0/TLS1.1 hellos before looking at this
       , tlsAllowedVersions = [ TLS12, TLS13 ]
         -- resumed handshakes skip the certificate and its RSA signature (several ms on a small VM)
       , tlsSessionManagerConfig = Just SM.defaultConfig
       })
      (setOnException quietHandshakes $ setPort port defaultSettings)
    $ logStdout (makeApplication env)
  case outcome of
    Left ()  -> gracefulShutdown config
    Right () -> die "Server terminated unexpectedly"

-- | Warp's exception printer, minus connections that never got past TLS:
-- scanners probing old protocols and cipher suites, and clients that connect
-- and send nothing (thousands a day). None of them reached the application.
quietHandshakes :: Maybe Request -> SomeException -> IO ()
quietHandshakes req e
  | Just (HandshakeFailed _) <- fromException e = pass
  | Just ClientClosedConnectionPrematurely <- fromException e = pass
  | otherwise = defaultOnException req e

-- | Tell the RPC server we are listening, once a second until it agrees.
notifyStart :: ClientConfig -> IO ()
notifyStart config = runRPCIO config clientStart >>= \case
  Right True -> pass
  _ -> threadDelay 1000000 >> notifyStart config

-- | Tell the RPC server we stop, then exit. The official client sends
-- client_stop once; we try three times, and never spin when the RPC server is
-- unreachable.
gracefulShutdown :: ClientConfig -> IO a
gracefulShutdown config = go (3 :: Int)
  where
    go attemptsLeft = runRPCIO config clientStop >>= \case
      Left _
        | attemptsLeft > 1 -> threadDelay 1000000 >> go (attemptsLeft - 1)
      _ -> exitSuccess

-- | Fetch a new certificate, retrying three times with exponential backoff
-- (1, 2, 4 seconds). Without a certificate we cannot go on: shut down.
refreshCerts :: ClientConfig -> IO ( CertificateChain, PrivKey )
refreshCerts config = go (0 :: Int)
  where
    go retries = runRPCIO config getCertificate >>= \case
      Right newCerts -> pure newCerts
      Left e
        | retries >= 3 -> do
          putStrLn $ "Failed to refresh certs: " <> show e
          putStrLn "Giving up after 3 retries"
          gracefulShutdown config
        | otherwise -> do
          let delaySeconds = 2 ^ retries :: Int
          putStrLn $ "Failed to refresh certs: " <> show e
          putStrLn
            $ "Retrying in " <> show delaySeconds <> " seconds (attempt " <> show (retries + 1) <> "/3)"
          threadDelay (1000000 * delaySeconds)
          go (retries + 1)
