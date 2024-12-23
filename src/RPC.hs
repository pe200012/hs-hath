{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module RPC
    ( RPCResponse(..)
    , parseRPCResponse
    , getPayload
    , RPC(..)
    , serverStat
    , stillAlive
    , clientStart
    , clientStop
    , clientLogin
    , checkGalleryTask
    , notifyGalleryCompletion
    , fetchGalleryFile
    , runRPC
    , runRPCIO
    ) where

import           API             hiding ( StillAlive )

import           Colog           ( Message, Severity(Info), richMessageAction )
import           Colog.Polysemy  ( Log, runLogAction )

import           Polysemy
import           Polysemy.Error  ( Error, errorToIOFinal )
import           Polysemy.Reader ( Reader, runReader )

import           Relude          hiding ( Reader, ask, runReader )

import           Servant.Client  ( ClientError )

import           Types

import           Utils           ( log )

data RPC m a where
    -- | Test if remote server is running
    ServerStat :: RPC m Bool
    -- | Heartbeat
    StillAlive :: RPC m ()
    -- | Notify server that client is ready to start a session
    ClientStart :: RPC m Bool
    -- | Notify server that client is stopping a session
    ClientStop :: RPC m Bool
    -- | Login to remote server
    ClientLogin :: RPC m HathSettings
    -- | Check if there is a gallery task to download
    CheckGalleryTask :: RPC m (Maybe GalleryMetadata)
    -- | Notify server that a gallery task has been completed
    NotifyGalleryCompletion :: GalleryMetadata -> RPC m ()
    -- | Fetch gallery files
    FetchGalleryFile :: ( GalleryMetadata, [ GalleryFile ] )
        -> RPC m [ ( GalleryFile, ByteString ) ] -- ^ Successfully downloaded files

makeSem ''RPC

runRPC :: forall a r.
       Members '[ Embed IO, Error RPCError, Reader ClientConfig, EHentaiAPI, Log Message ] r
       => Sem (RPC ': r) a
       -> Sem r a
runRPC = interpret $ \case
    ServerStat -> do
        b <- checkServerStatus
        log Info $ "Server availablity: " <> show b
        return b
    StillAlive -> log Info "Tell server that client is still alive" >> heartbeat
    ClientStart -> log Info "Trying to start a session" >> startListening
    ClientStop -> log Info "Stopping a session" >> stopListening
    ClientLogin -> log Info "Logging in" >> login
    CheckGalleryTask -> log Info "Checking for a gallery task" >> nextGalleryTask
    NotifyGalleryCompletion
        metadata -> log Info "Notifying server that a gallery task has been completed"
        >> completeGalleryTask metadata
    FetchGalleryFile
        ( metadata, files ) -> log Info "Fetching gallery files" >> phi metadata [] files
  where
    phi _ acc [] = pure acc
    phi metadata acc (x : xs) = do
        maybeContent <- downloadGalleryFile metadata x
        case maybeContent of
            Just content -> phi metadata (( x, content ) : acc) xs
            Nothing      -> phi metadata acc xs

{-# INLINE runRPCIO #-}
runRPCIO :: ClientConfig
         -> Sem
             '[ RPC
              , EHentaiAPI
              , Log Message
              , Reader ClientConfig
              , Embed IO
              , Error ClientError
              , Error RPCError
              , Final IO
              ]
             a
         -> IO (Either RPCError (Either ClientError a))
runRPCIO cfg
    = runFinal
    . errorToIOFinal @RPCError
    . errorToIOFinal @ClientError
    . embedToFinal @IO
    . runReader cfg
    . runLogAction @IO richMessageAction
    . runEHentaiAPI
    . runRPC

