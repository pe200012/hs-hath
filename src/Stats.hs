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
  , readPrometheus
  , runStats
  ) where

import qualified Control.Concurrent.STM as STM

import qualified Data.Text              as T
import           Data.Time.Clock        ( UTCTime, getCurrentTime )

import           Polysemy
import           Polysemy.Operators
import           Polysemy.Reader        ( Reader, ask )

import           Relude                 hiding ( Reader, ask )

data TrafficStats
  = TrafficStats
  { uploadBytes :: !Int64, downloadBytes :: !Int64, servedCount :: !Int64, fetchedCount :: !Int64 }
  deriving ( Show, Eq, Generic )

emptyTrafficStats :: TrafficStats
emptyTrafficStats = TrafficStats 0 0 0 0

data StatsEnv = StatsEnv { statsStart :: !UTCTime, statsVar :: !(TVar TrafficStats) }

newStatsEnv :: IO StatsEnv
newStatsEnv = do
  t0 <- getCurrentTime
  v <- STM.newTVarIO emptyTrafficStats
  pure $ StatsEnv t0 v

data Stats m a where
  AddUpload :: Int -> Stats m ()
  AddDownload :: Int -> Stats m ()
  IncServed :: Stats m ()
  IncFetched :: Stats m ()
  ReadStats :: Stats m TrafficStats
  ReadPrometheus :: Stats m Text

  -- New uptime metric
  UpdateUptime :: Stats m ()

  -- Active connections
  IncActiveConnections :: Stats m ()
  DecActiveConnections :: Stats m ()

  -- Gallery Downloader metrics
  IncDlTask :: Stats m ()
  IncDlFile :: Stats m ()
  AddDlBytes :: Int -> Stats m ()

  -- Error metrics with labels
  IncError :: Text -> Stats m ()  -- error type label

makeSem ''Stats

toPrometheus :: TrafficStats -> Text
toPrometheus s
  = T.unlines
    [ "# HELP hs_hath_upload_bytes_total Total bytes uploaded"
    , "# TYPE hs_hath_upload_bytes_total counter"
    , "hs_hath_upload_bytes_total " <> show (uploadBytes s)
    , ""
    , "# HELP hs_hath_download_bytes_total Total bytes downloaded"
    , "# TYPE hs_hath_download_bytes_total counter"
    , "hs_hath_download_bytes_total " <> show (downloadBytes s)
    , ""
    , "# HELP hs_hath_served_requests_total Total served requests"
    , "# TYPE hs_hath_served_requests_total counter"
    , "hs_hath_served_requests_total " <> show (servedCount s)
    , ""
    , "# HELP hs_hath_fetched_requests_total Total fetched requests"
    , "# TYPE hs_hath_fetched_requests_total counter"
    , "hs_hath_fetched_requests_total " <> show (fetchedCount s)
    ]

runStats :: Members '[ Embed IO, Reader StatsEnv ] r => Stats : r @> a -> r @> a
runStats = interpret $ \case
  AddUpload n    -> do
    StatsEnv { statsVar } <- ask
    embed $ STM.atomically $ STM.modifyTVar' statsVar $ \s -> s
      { uploadBytes = uploadBytes s + fromIntegral n }
  AddDownload n  -> do
    StatsEnv { statsVar } <- ask
    embed $ STM.atomically $ STM.modifyTVar' statsVar $ \s -> s
      { downloadBytes = downloadBytes s + fromIntegral n }
  IncServed      -> do
    StatsEnv { statsVar } <- ask
    embed $ STM.atomically $ STM.modifyTVar' statsVar $ \s -> s { servedCount = servedCount s + 1 }
  IncFetched     -> do
    StatsEnv { statsVar } <- ask
    embed $ STM.atomically $ STM.modifyTVar' statsVar $ \s -> s
      { fetchedCount = fetchedCount s + 1 }
  ReadStats      -> do
    StatsEnv { statsVar } <- ask
    embed $ STM.readTVarIO statsVar
  ReadPrometheus -> do
    StatsEnv { statsVar } <- ask
    s <- embed $ STM.readTVarIO statsVar
    pure $ toPrometheus s
