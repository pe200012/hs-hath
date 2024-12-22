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

import           Polysemy
import           Polysemy.Error  ( Error, errorToIOFinal )
import           Polysemy.Reader ( Reader, runReader )

import           Relude          hiding ( Reader, ask, runReader )

import           Servant.Client  ( ClientError )

import           Types

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

runRPC :: forall a r. Members '[ Embed IO, Error RPCError, Reader ClientConfig, EHentaiAPI ] r
       => Sem (RPC ': r) a
       -> Sem r a
runRPC = interpret $ \case
    ServerStat -> checkServerStatus
    StillAlive -> heartbeat
    ClientStart -> startListening
    ClientStop -> stopListening
    ClientLogin -> login
    CheckGalleryTask -> nextGalleryTask
    NotifyGalleryCompletion metadata -> completeGalleryTask metadata
    FetchGalleryFile ( metadata, files ) -> phi metadata [] files
  where
    phi _ acc [] = pure acc
    phi metadata acc (x : xs) = do
        maybeContent <- downloadGalleryFile metadata x
        case maybeContent of
            Just content -> phi metadata (( x, content ) : acc) xs
            Nothing      -> phi metadata acc xs

runRPCIO :: ClientConfig
         -> Sem
             '[ RPC
              , EHentaiAPI
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
    . runEHentaiAPI
    . runRPC

