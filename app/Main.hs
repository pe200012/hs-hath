module Main ( main ) where

import           CLI          ( Options(..), applyOptionsToConfig, parseOptions )

import           Genesis      ( fetchCertificate, runGenesisIO )

import           RPC

import           Relude       hiding ( runReader )

import           Server       ( ServerAction(GracefulShutdown), startServer )

import           System.Posix ( Handler(Catch), installHandler, sigINT, sigTERM )

import           Types

main :: IO ()
main = do
    options <- parseOptions
    baseConfig <- readClientConfig (toText $ optConfigPath options)
    let config = applyOptionsToConfig options baseConfig
    chan <- newEmptyMVar
    void $ installHandler sigINT (Catch $ putMVar chan GracefulShutdown) Nothing
    void $ installHandler sigTERM (Catch $ putMVar chan GracefulShutdown) Nothing
    runRPCIO config serverStat >>= \case
        Right (Right True) -> runRPCIO config clientLogin >>= \case
            Right (Right settings) -> runGenesisIO config fetchCertificate >>= \case
                Right (Right certs) -> startServer config settings certs chan
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

