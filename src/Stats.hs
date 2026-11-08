{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Stats
  ( StatsEnv(..)
  , newStatsEnv
  , Stats(..)
  , addUpload
  , addDownload
  , incServed
  , incFetched
  , updateUptime
  , incActiveConnections
  , decActiveConnections
  , incDlTask
  , incDlFile
  , addDlBytes
  , readPrometheus
  , runStats
  ) where

import           GHC.Clock          ( getMonotonicTime )

import qualified Metrics.Counter    as Counter
import qualified Metrics.Gauge      as Gauge
import           Metrics.Registry   ( newRegistry, registerCounter, registerGauge, registerHistogram )
import           Metrics.Snapshot   ( getCachedSnapshot, startSnapshotWorker )
-- Custom metrics implementation (replaces prometheus library)
import           Metrics.Types      ( Counter, Gauge, Histogram, Registry )

import           Polysemy
import           Polysemy.Operators
import           Polysemy.Reader    ( Reader, ask )

import           Relude             hiding ( Reader, ask )

data StatsEnv
  = StatsEnv
  { statsStart :: !Double
  , statsRegistry :: !Registry
  , statsUploadBytesCounter :: !Counter
  , statsDownloadBytesCounter :: !Counter
  , statsServedCounter :: !Counter
  , statsFetchedCounter :: !Counter
  , statsActiveConnectionsGauge :: !Gauge
  , statsUptimeGauge :: !Gauge
  , statsDlTaskCounter :: !Counter
  , statsDlFileCounter :: !Counter
  , statsDlBytesCounter :: !Counter
  , statsLatency :: !Histogram
  }

newStatsEnv :: IO StatsEnv
newStatsEnv = do
  t0 <- getMonotonicTime
  registry <- newRegistry
  uploadBytesCounter
    <- registerCounter "hath_cache_sent_bytes_total" "Total bytes sent from cache" registry
  downloadBytesCounter
    <- registerCounter "hath_cache_received_bytes_total" "Total bytes received into cache" registry
  servedCounter <- registerCounter "hath_cache_sent_total" "Total number of cache hits" registry
  fetchedCounter
    <- registerCounter "hath_cache_received_total" "Total number of cache misses" registry
  activeConnectionsGauge
    <- registerGauge "hath_active_connections" "Number of active connections" registry
  uptimeGauge <- registerGauge "hath_uptime_seconds" "Server uptime in seconds" registry
  dlTaskCounter
    <- registerCounter "hath_download_task_count_total" "Total gallery download tasks" registry
  dlFileCounter
    <- registerCounter "hath_download_file_count_total" "Total gallery files downloaded" registry
  dlBytesCounter <- registerCounter
    "hath_download_size_bytes_total"
    "Total bytes downloaded from galleries"
    registry
  -- includes sending the body, so large files land in the upper buckets
  latency <- registerHistogram
    "hath_request_duration_seconds"
    "Time from request to the last byte sent"
    [ 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 30, 60 ]
    registry

  -- Start background snapshot worker (updates every 5 seconds)
  startSnapshotWorker registry (5 * 1000000)

  pure
    $ StatsEnv
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
      latency

data Stats m a where
  AddUpload :: Int -> Stats m ()
  AddDownload :: Int -> Stats m ()
  IncServed :: Stats m ()
  IncFetched :: Stats m ()
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

makeSem ''Stats

runStats :: Members '[ Embed IO, Reader StatsEnv ] r => Stats : r @> a -> r @> a
runStats = interpret $ \case
  AddUpload n -> do
    env <- ask
    embed $ Counter.add n (statsUploadBytesCounter env)
  AddDownload n -> do
    env <- ask
    embed $ Counter.add n (statsDownloadBytesCounter env)
  IncServed -> do
    env <- ask
    embed $ Counter.inc (statsServedCounter env)
  IncFetched -> do
    env <- ask
    embed $ Counter.inc (statsFetchedCounter env)

  -- New uptime metric
  UpdateUptime -> do
    env <- ask
    now <- embed getMonotonicTime
    let elapsed = now - statsStart env
    embed $ Gauge.set (round elapsed) (statsUptimeGauge env)

  -- Active connections
  IncActiveConnections -> do
    env <- ask
    embed $ Gauge.inc (statsActiveConnectionsGauge env)

  DecActiveConnections -> do
    env <- ask
    embed $ Gauge.dec (statsActiveConnectionsGauge env)

  -- Gallery Downloader metrics
  IncDlTask -> do
    env <- ask
    embed $ Counter.inc (statsDlTaskCounter env)
  IncDlFile -> do
    env <- ask
    embed $ Counter.inc (statsDlFileCounter env)
  AddDlBytes n -> do
    env <- ask
    embed $ Counter.add n (statsDlBytesCounter env)

  ReadPrometheus -> do
    env <- ask
    snapshot <- embed $ getCachedSnapshot (statsRegistry env)
    pure $ decodeUtf8 snapshot

