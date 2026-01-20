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

import           Data.Binary.Builder                           ( toLazyByteString )
import           Data.Time.Clock                               ( UTCTime
                                                               , diffUTCTime
                                                               , getCurrentTime
                                                               )

import           Polysemy
import           Polysemy.Operators
import           Polysemy.Reader                               ( Reader, ask )

import           Relude                                        hiding ( Reader, ask )

import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import           System.Metrics.Prometheus.Concurrent.Registry ( new
                                                               , registerCounter
                                                               , registerGauge
                                                               )
import           System.Metrics.Prometheus.Encode.Text         ( encodeMetrics )
import           System.Metrics.Prometheus.Metric.Counter      ( Counter )
import qualified System.Metrics.Prometheus.Metric.Counter      as Counter
import           System.Metrics.Prometheus.Metric.Gauge        ( Gauge )
import qualified System.Metrics.Prometheus.Metric.Gauge        as Gauge

data StatsEnv
  = StatsEnv
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
  registry <- new
  uploadBytesCounter <- registerCounter "hath_cache_sent_bytes_total" mempty registry
  downloadBytesCounter <- registerCounter "hath_cache_received_bytes_total" mempty registry
  servedCounter <- registerCounter "hath_cache_sent_total" mempty registry
  fetchedCounter <- registerCounter "hath_cache_received_total" mempty registry
  activeConnectionsGauge <- registerGauge "hath_active_connections" mempty registry
  uptimeGauge <- registerGauge "hath_uptime_seconds" mempty registry
  dlTaskCounter <- registerCounter "hath_download_task_count_total" mempty registry
  dlFileCounter <- registerCounter "hath_download_file_count_total" mempty registry
  dlBytesCounter <- registerCounter "hath_download_size_bytes_total" mempty registry
  errorCounter <- registerCounter "hath_errors_total" mempty registry
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
      errorCounter

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

  -- Error metrics with labels
  IncError :: Text -> Stats m ()  -- error type label

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
    now <- embed getCurrentTime
    let elapsed = realToFrac (diffUTCTime now (statsStart env)) :: Double
    embed $ Gauge.set elapsed (statsUptimeGauge env)

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

  -- Error metrics with labels
  IncError _errorType -> do
    env <- ask
    embed $ Counter.inc (statsErrorCounter env)

  ReadPrometheus -> do
    env <- ask
    sample <- embed $ Registry.sample (statsRegistry env)
    pure $ decodeUtf8 $ toLazyByteString $ encodeMetrics sample
