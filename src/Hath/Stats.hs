{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The counters behind @\/metrics@, and the effect the rest of the client
-- counts through.
module Hath.Stats
  ( StatsEnv(..)
  , newStatsEnv
  , Stats(..)
  , addUpload
  , addDownload
  , incServed
  , incFetched
  , incDlTask
  , incDlFile
  , addDlBytes
  , readPrometheus
  , runStats
  ) where

import qualified Hath.Metrics.Counter  as Counter
import           Hath.Metrics.Registry ( newRegistry, registerCounter, registerGauge, registerHistogram )
import           Hath.Metrics.Snapshot ( getCachedSnapshot, startSnapshotWorker )
import           Hath.Metrics.Types    ( Counter, Gauge, Histogram, Registry )

import           Polysemy
import           Polysemy.Reader       ( Reader, ask )

import           Relude                hiding ( Reader, ask )

data StatsEnv
  = StatsEnv
  { statsRegistry :: !Registry
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

-- | Register the metrics and start the worker that renders them every 5 seconds.
newStatsEnv :: IO StatsEnv
newStatsEnv = do
  registry <- newRegistry
  uploadBytes <- registerCounter "hath_cache_sent_bytes_total" "Total bytes sent from cache" registry
  downloadBytes
    <- registerCounter "hath_cache_received_bytes_total" "Total bytes received into cache" registry
  served <- registerCounter "hath_cache_sent_total" "Total number of cache hits" registry
  fetched <- registerCounter "hath_cache_received_total" "Total number of cache misses" registry
  activeConnections <- registerGauge "hath_active_connections" "Number of active connections" registry
  uptime <- registerGauge "hath_uptime_seconds" "Server uptime in seconds" registry
  dlTasks <- registerCounter "hath_download_task_count_total" "Total gallery download tasks" registry
  dlFiles <- registerCounter "hath_download_file_count_total" "Total gallery files downloaded" registry
  dlBytes
    <- registerCounter "hath_download_size_bytes_total" "Total bytes downloaded from galleries" registry
  -- includes sending the body, so large files land in the upper buckets
  latency <- registerHistogram
    "hath_request_duration_seconds"
    "Time from request to the last byte sent"
    [ 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 30, 60 ]
    registry
  startSnapshotWorker registry (5 * 1000000)
  pure
    StatsEnv
    { statsRegistry = registry
    , statsUploadBytesCounter = uploadBytes
    , statsDownloadBytesCounter = downloadBytes
    , statsServedCounter = served
    , statsFetchedCounter = fetched
    , statsActiveConnectionsGauge = activeConnections
    , statsUptimeGauge = uptime
    , statsDlTaskCounter = dlTasks
    , statsDlFileCounter = dlFiles
    , statsDlBytesCounter = dlBytes
    , statsLatency = latency
    }

data Stats m a where
  -- | Bytes sent to readers.
  AddUpload :: Int -> Stats m ()
  -- | Bytes fetched from upstream into the cache.
  AddDownload :: Int -> Stats m ()
  -- | One file served.
  IncServed :: Stats m ()
  -- | One file fetched from upstream.
  IncFetched :: Stats m ()
  -- | One look at the gallery download queue.
  IncDlTask :: Stats m ()
  -- | One gallery file downloaded, and its size.
  IncDlFile :: Stats m ()
  AddDlBytes :: Int -> Stats m ()
  -- | The metrics in Prometheus text format, as last rendered.
  ReadPrometheus :: Stats m Text

makeSem ''Stats

runStats :: Members '[ Embed IO, Reader StatsEnv ] r => Sem (Stats : r) a -> Sem r a
runStats = interpret $ \case
  AddUpload n    -> count (Counter.add n . statsUploadBytesCounter)
  AddDownload n  -> count (Counter.add n . statsDownloadBytesCounter)
  IncServed      -> count (Counter.inc . statsServedCounter)
  IncFetched     -> count (Counter.inc . statsFetchedCounter)
  IncDlTask      -> count (Counter.inc . statsDlTaskCounter)
  IncDlFile      -> count (Counter.inc . statsDlFileCounter)
  AddDlBytes n   -> count (Counter.add n . statsDlBytesCounter)
  ReadPrometheus -> decodeUtf8 <$> count (getCachedSnapshot . statsRegistry)
  where
    count :: Members '[ Embed IO, Reader StatsEnv ] r => (StatsEnv -> IO x) -> Sem r x
    count f = ask >>= embed . f
