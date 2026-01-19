module Main ( main ) where

import           CLI                    ( Options(..), applyOptionsToConfig, parseOptions )

import qualified Data.Text              as T

import           Database.SQLite.Simple ( withConnection )

import           FileVerification       ( VerificationStats(..), verifyAllFiles )

import           HathNetwork.Genesis    ( fetchCertificate, runGenesisIO )
import           HathNetwork.RPC

import           Relude                 hiding ( runReader )

import           Server                 ( ServerAction(GracefulShutdown), startServer )

import           Storage.Database       ( initializeDB )

import           System.Posix           ( Handler(Catch), installHandler, sigINT, sigTERM )

import           Types

import           Version                ( versionInfo )

main :: IO ()
main = do
  options <- parseOptions

  -- Handle --version flag
  when (optShowVersion options) $ do
    putStrLn $ T.unpack versionInfo
    exitSuccess

  baseConfig <- readClientConfig (toText $ optConfigPath options)
  let config = applyOptionsToConfig options baseConfig

  -- Initialize database and run startup verification unless skipped
  withConnection (T.unpack $ cachePath config) $ \conn -> do
    initializeDB conn

    -- Run full cache verification by default (skip with --skip-startup-verify)
    unless (optSkipStartupVerify options) $ do
      putStrLn "Verifying cache integrity (this may take a while)..."
      stats <- verifyAllFiles conn
      putStrLn $ "Verification complete:"
      putStrLn $ "  Files verified: " <> show (verifiedCount stats)
      putStrLn $ "  Corrupted files removed: " <> show (corruptedCount stats)
      putStrLn $ "  Time elapsed: " <> show (verificationTime stats)
      when (corruptedCount stats > 0)
        $ putStrLn
        $ "WARNING: " <> show (corruptedCount stats) <> " corrupted files were found and removed."

  chan <- newEmptyMVar
  void $ installHandler sigINT (Catch $ putMVar chan GracefulShutdown) Nothing
  void $ installHandler sigTERM (Catch $ putMVar chan GracefulShutdown) Nothing

  runRPCIO config serverStat >>= \case
    Right (Right True) -> runRPCIO config clientLogin >>= \case
      Right (Right settings) -> runGenesisIO config fetchCertificate >>= \case
        Right (Right certs) -> startServer
          config
          settings
          certs
          chan
          (optSkipPeriodicVerify options)
          (optDisableRateLimit options)
          (optTrustProxyHeaders options)
        e -> do
          print e
          putStrLn "Unable to fetch certs, exiting..."
          exitFailure
      e -> do
        print e
        putStrLn "Unable to login, exiting..."
        exitFailure
    e -> do
      print e
      putStrLn "RPC is not available, exiting..."
      exitFailure

