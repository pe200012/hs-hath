{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Stats
    ( TrafficStats(..)
    , emptyTrafficStats
    , StatsEnv(..)
    , newStatsEnv
    , Stats(..)
    , addUpload
    , addDownload
    , incServed
    , incFetched
    , readStats
    , StatsSnapshot(..)
    , readSnapshot
    , runStats
    ) where

import qualified Control.Concurrent.STM as STM

import           Data.Aeson             ( (.=), ToJSON, object )
import qualified Data.Aeson             as A
import           Data.Time.Clock        ( UTCTime, getCurrentTime )

import           Polysemy
import           Polysemy.Operators
import           Polysemy.Reader        ( Reader, ask )

import           Relude                 hiding ( Reader, ask )

data TrafficStats
    = TrafficStats { uploadBytes   :: !Int64
                   , downloadBytes :: !Int64
                   , servedCount   :: !Int64
                   , fetchedCount  :: !Int64
                   }
    deriving ( Show, Eq, Generic )

emptyTrafficStats :: TrafficStats
emptyTrafficStats = TrafficStats 0 0 0 0

data StatsEnv = StatsEnv { statsStart :: !UTCTime, statsVar :: !(TVar TrafficStats) }

newStatsEnv :: IO StatsEnv
newStatsEnv = do
    t0 <- getCurrentTime
    v <- STM.newTVarIO emptyTrafficStats
    pure $ StatsEnv t0 v

data StatsSnapshot
    = StatsSnapshot { since          :: !UTCTime
                    , upload_bytes   :: !Int64
                    , download_bytes :: !Int64
                    , served_count   :: !Int64
                    , fetched_count  :: !Int64
                    }
    deriving ( Show, Eq, Generic )

instance ToJSON StatsSnapshot where
    toJSON s
        = object
            [ "since" .= since s
            , "upload_bytes" .= upload_bytes s
            , "download_bytes" .= download_bytes s
            , "served_count" .= served_count s
            , "fetched_count" .= fetched_count s
            ]

data Stats m a where
    AddUpload :: Int -> Stats m ()
    AddDownload :: Int -> Stats m ()
    IncServed :: Stats m ()
    IncFetched :: Stats m ()
    ReadStats :: Stats m TrafficStats
    ReadSnapshot :: Stats m StatsSnapshot

makeSem ''Stats

runStats :: Members '[ Embed IO, Reader StatsEnv ] r => Stats : r @> a -> r @> a
runStats = interpret $ \case
    AddUpload n   -> do
        StatsEnv { statsVar } <- ask
        embed $ STM.atomically $ STM.modifyTVar' statsVar $ \s -> s
            { uploadBytes = uploadBytes s + fromIntegral n }
    AddDownload n -> do
        StatsEnv { statsVar } <- ask
        embed $ STM.atomically $ STM.modifyTVar' statsVar $ \s -> s
            { downloadBytes = downloadBytes s + fromIntegral n }
    IncServed     -> do
        StatsEnv { statsVar } <- ask
        embed $ STM.atomically $ STM.modifyTVar' statsVar $ \s -> s
            { servedCount = servedCount s + 1 }
    IncFetched    -> do
        StatsEnv { statsVar } <- ask
        embed $ STM.atomically $ STM.modifyTVar' statsVar $ \s -> s
            { fetchedCount = fetchedCount s + 1 }
    ReadStats     -> do
        StatsEnv { statsVar } <- ask
        embed $ STM.readTVarIO statsVar
    ReadSnapshot  -> do
        StatsEnv { statsStart, statsVar } <- ask
        s <- embed $ STM.readTVarIO statsVar
        pure
            $ StatsSnapshot { since          = statsStart
                            , upload_bytes   = uploadBytes s
                            , download_bytes = downloadBytes s
                            , served_count   = servedCount s
                            , fetched_count  = fetchedCount s
                            }
