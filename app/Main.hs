module Main ( main ) where

import           Genesis             ( fetchCertificate, runGenesisIO )

import           RPC

import           Relude              hiding ( runReader )

import           Server              ( ServerAction(GracefulShutdown), startServer )

import           System.Posix        ( Handler(Catch), installHandler, sigINT, sigTERM )

import           Types

import           UnliftIO.Concurrent ( myThreadId, threadDelay, throwTo )

main :: IO ()
main = do
    config <- readClientConfig "./client-login"
    chan <- newEmptyMVar
    void $ installHandler sigINT (Catch $ putMVar chan GracefulShutdown) Nothing
    void $ installHandler sigTERM (Catch $ putMVar chan GracefulShutdown) Nothing
    runRPCIO config serverStat >>= \case
        Right (Right True) -> runRPCIO config clientLogin >>= \case
            Right (Right settings) -> runGenesisIO config fetchCertificate >>= \case
                Right (Right certs) -> startServer config settings certs chan 8443
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

