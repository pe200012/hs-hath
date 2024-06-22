
{-# LANGUAGE DataKinds #-}

module Main ( main ) where

import           Colog                   ( logError, logInfo, richMessageAction, usingLoggerT )

import           Control.Concurrent      ( forkIO, myThreadId, threadDelay, throwTo )
import           Control.Exception       ( catch )

import           Data.String.Interpolate ( i )

import qualified Dhall

import           Network.HTTP.Types      ( status200 )

import           Query

import           Relude

import           Server                  ( runHTTPServer )

import           System.Posix            ( Handler(Catch), installHandler, sigINT, sigTERM )

import           Types                   ( GracefulShutdown(GracefulShutdown) )

import           Version                 ( versionString )

hathMain :: IO ()
hathMain = do
    config <- Dhall.input Dhall.auto "./client-login"
    res <- usingLoggerT richMessageAction serverStatus
    when (res /= status200) $ do
        usingLoggerT richMessageAction $ logError "Server status request failed"
        exitFailure
    clientLogin config
    sts <- usingLoggerT richMessageAction $ Query.hathSettings config
    hSets <- newIORef sts
    usingLoggerT richMessageAction $ logInfo "Starting server"
    myId <- myThreadId
    serverId <- forkIO $ runHTTPServer myId hSets config
    void $ installHandler sigINT (Catch $ do
                                      usingLoggerT richMessageAction $ clientStop config
                                      throwTo serverId GracefulShutdown
                                      throwTo myId GracefulShutdown) Nothing
    void $ installHandler sigTERM (Catch $ do
                                       usingLoggerT richMessageAction $ clientStop config
                                       throwTo serverId GracefulShutdown
                                       throwTo myId GracefulShutdown) Nothing
    threadDelay (1 * periodSeconds)
    usingLoggerT richMessageAction $ clientStart config
    forever $ do
        threadDelay (60 * periodSeconds)
        heartBeat config `catch` print @SomeException
  where
    periodSeconds = 1000000

main :: IO ()
main = do
    usingLoggerT richMessageAction
        $ logInfo ([i|Starting Hentai@Home server. Version: #{versionString}|] :: Text)
    hathMain
