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

import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import           System.Metrics.Prometheus.Concurrent.RegistryT
import           System.Metrics.Prometheus.Metric.Counter
import           System.Metrics.Prometheus.Metric.Gauge
import qualified System.Metrics.Prometheus.MetricId as MetricId
import qualified System.Metrics.Prometheus.Encode.Text as Encode

import           Relude                 hiding ( Reader, ask )

data TrafficStats
  = TrafficStats
  { uploadBytes :: !Int64
  , downloadBytes :: !Int64
  , servedCount :: !Int64
  , fetchedCount :: !Int64
  , dlTaskCount   :: !Int64
  , dlFileCount   :: !Int64
  , dlBytes       :: !Int64
  } deriving ( Show, Eq, Generic )

emptyTrafficStats :: TrafficStats
emptyTrafficStats = TrafficStats 0 0 0 0 0 0 0

data StatsEnv = StatsEnv
  { statsStart :: !UTCTime
  , statsRegistry :: !Registry.Registry
  , statsUploadBytesCounter :: !Counter
  , statsDownloadBytesCounter :: !Counter
  , statsServedCounter :: !Counter
  , statsFetchedCounter :: !Counter
  , statsActiveConnectionsGauge :: !Gauge
  , statsUptimeGauge :: !Gauge
  , statsDlTaskCounter :: !Counter
  , statsDlFileCounter :: !Counter
  , statsDlBytesCounter :: !Counter
  , statsErrorCounter :: !Counter
  }

newStatsEnv :: IO StatsEnv
newStatsEnv = do
  t0 <- getCurrentTime
  registry <- Registry.new
  uploadBytesCounter <- Registry.registerCounter "hath_cache_sent_bytes_total" mempty registry
  downloadBytesCounter <- Registry.registerCounter "hath_cache_received_bytes_total" mempty registry
  servedCounter <- Registry.registerCounter "hath_cache_sent_total" mempty registry
  fetchedCounter <- Registry.registerCounter "hath_cache_received_total" mempty registry
  activeConnectionsGauge <- Registry.registerGauge "hath_active_connections" mempty registry
  uptimeGauge <- Registry.registerGauge "hath_uptime_seconds" mempty registry
  dlTaskCounter <- Registry.registerCounter "hath_download_task_count_total" mempty registry
  dlFileCounter <- Registry.registerCounter "hath_download_file_count_total" mempty registry
  dlBytesCounter <- Registry.registerCounter "hath_download_size_bytes_total" mempty registry
  errorCounter <- Registry.registerCounter "hath_errors_total" (MetricId.fromList [("type", "rpc_timeout")]) registry
  pure $ StatsEnv
    t0
    registry
    uploadBytesCounter
    downloadBytesCounter
    servedCounter
    fetchedCounter
    activeConnectionsGauge
    uptimeGauge
    dlTaskCounter
    dlFileCounter
    dlBytesCounter
    errorCounter

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
