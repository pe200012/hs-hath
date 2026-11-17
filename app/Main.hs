module Main ( main ) where

import           Database.SQLite.Simple ( withConnection )

import           Hath.CLI               ( Options(..), applyOptionsToConfig, parseOptions )
import           Hath.Cache.SQLite      ( VerificationStats(..), initializeDB, verifyAllFiles )
import           Hath.Config            ( CacheBackend(..), ClientConfig(..), readClientConfig )
import           Hath.RPC               ( clientLogin, getCertificate, runRPCIO, serverStat )
import           Hath.Server            ( ServerAction(GracefulShutdown)
                                        , ServerOptions(..)
                                        , needsRenewal
                                        , startServer
                                        )
import           Hath.ServerTime        ( serverNow )

import           Relude

import           System.Posix           ( Handler(Catch), installHandler, sigINT, sigTERM )

import           UnliftIO               ( newTChanIO, writeTChan )

main :: IO ()
main = do
  options <- parseOptions
  config <- applyOptionsToConfig options <$> readClientConfig (toText $ optConfigPath options)
  when (optValidateConfig options) $ do
    putStrLn "Configuration is valid."
    exitSuccess

  -- for the other backends cachePath is a directory, or not used at all
  when (cacheBackend config == CacheBackendSQLite) $ prepareSQLiteCache options config

  actions <- newTChanIO
  forM_ [ sigINT, sigTERM ] $ \signal
    -> installHandler signal (Catch $ atomically $ writeTChan actions GracefulShutdown) Nothing

  runRPCIO config serverStat >>= either (`giveUp` "RPC is not available, exiting...") pure
  settings <- runRPCIO config clientLogin >>= either (`giveUp` "Unable to login, exiting...") pure
  certs <- runRPCIO config getCertificate
    >>= either (`giveUp` "Unable to fetch certs, exiting...") pure
  now <- serverNow
  when (needsRenewal now (fst certs)) $ do
    putStrLn "The certificate from the server expires within a day; try again later."
    exitFailure
  startServer
    config
    ServerOptions { disableRateLimit  = optDisableRateLimit options
                  , trustProxyHeaders = optTrustProxyHeaders options
                  }
    settings
    certs
    actions

prepareSQLiteCache :: Options -> ClientConfig -> IO ()
prepareSQLiteCache options config = withConnection (toString $ cachePath config) $ \conn -> do
  initializeDB conn
  unless (optSkipStartupVerify options) $ do
    putStrLn "Verifying cache integrity (this may take a while)..."
    stats <- verifyAllFiles conn
    putStrLn "Verification complete:"
    putStrLn $ "  Files verified: " <> show (verifiedCount stats)
    putStrLn $ "  Corrupted files removed: " <> show (corruptedCount stats)
    putStrLn $ "  Time elapsed: " <> show (verificationTime stats)
    when (corruptedCount stats > 0)
      $ putStrLn
      $ "WARNING: " <> show (corruptedCount stats) <> " corrupted files were found and removed."

giveUp :: Show failure => failure -> String -> IO a
giveUp failure message = do
  print failure
  putStrLn message
  exitFailure
