
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main ( main ) where

import           Colog                   ( logError, logInfo, richMessageAction, usingLoggerT )

import           Control.Concurrent      ( forkIO, myThreadId, threadDelay, throwTo )
import           Control.Exception       ( SomeException, catch )
import           Control.Monad           ( forever, void, when )

import           Data.IORef              ( newIORef )
import           Data.String.Interpolate ( i )
import           Data.Text               ( Text )

import qualified Dhall

import           Network.HTTP.Types      ( status200 )

import           Prelude                 hiding ( log )

import           Query

import           Server

import           System.Exit             ( exitFailure )
import           System.Posix            ( Handler(Catch), installHandler, sigINT, sigTERM )

import           Types

hathMain :: IO ()
hathMain = do
    config <- unmarshallClientConfig <$> Dhall.input Dhall.auto "./client-login"
    res <- usingLoggerT richMessageAction serverStatus
    when (res /= status200) $ do
        usingLoggerT richMessageAction $ logError "Server status request failed"
        exitFailure
    clientLogin config
    sts <- usingLoggerT richMessageAction $ Query.hathSettings config
    hSets <- newIORef sts
    usingLoggerT richMessageAction $ logInfo "Starting server"
    myId <- myThreadId
    serverId <- forkIO $ runHTTPServer hSets config
    void $ installHandler sigINT (Catch $ do
                                      usingLoggerT richMessageAction $ clientStop config
                                      throwTo serverId GracefulShutdown
                                      throwTo myId GracefulShutdown) Nothing
    void $ installHandler sigTERM (Catch $ do
                                       usingLoggerT richMessageAction $ clientStop config
                                       throwTo serverId GracefulShutdown
                                       throwTo myId GracefulShutdown) Nothing
    usingLoggerT richMessageAction $ clientStart config
    forever $ do
        threadDelay (60 * periodSeconds)
        heartBeat config `catch` print @SomeException
  where
    periodSeconds = 1000000

main :: IO ()
main = do
    usingLoggerT richMessageAction
        $ logInfo
            ([i|Starting Hentai@Home server. Version: #{(__DATE__ :: String)} #{(__TIME__ :: String)}|]
                 :: Text)
    hathMain
