module Main ( main ) where

import           Genesis ( fetchCertificate, runGenesisIO )

import           RPC

import           Relude  hiding ( runReader )

import           Server  ( startServer )

import           Types

main :: IO ()
main = do
    config <- readClientConfig "./client-login"
    runRPCIO config serverStat >>= \case
        Right (Right True) -> runRPCIO config clientLogin >>= \case
            Right (Right settings) -> runGenesisIO config fetchCertificate >>= \case
                Right (Right certs) -> startServer config settings certs 8443
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

