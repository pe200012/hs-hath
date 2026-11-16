{-# LANGUAGE DataKinds #-}

-- | The counters behind @\/metrics@, and their rendering in the Prometheus
-- text format.
module Hath.Stats
  ( StatsEnv
  , newStatsEnv
    -- * Counting
  , addUpload
  , addDownload
  , incServed
  , incFetched
  , incDlTask
  , incDlFile
  , addDlBytes
  , trackConnection
  , observeDuration
    -- * Rendering
  , readMetrics
  , renderMetrics
  ) where

import           Control.Exception ( finally )

import           GHC.Clock         ( getMonotonicTime )

import           Numeric           ( showFFloat )

import           Polysemy
import           Polysemy.Reader   ( Reader, ask )

import           Relude            hiding ( Reader, ask )

data StatsEnv
  = StatsEnv
  { statsStart :: !Double
    -- ^ monotonic seconds; the uptime counts from here
  , statsSentBytes :: !(IORef Int64)
  , statsReceivedBytes :: !(IORef Int64)
  , statsSent :: !(IORef Int64)
  , statsReceived :: !(IORef Int64)
  , statsActiveConnections :: !(IORef Int64)
  , statsDlTasks :: !(IORef Int64)
  , statsDlFiles :: !(IORef Int64)
  , statsDlBytes :: !(IORef Int64)
  , statsDurations :: ![ IORef Int64 ]
    -- ^ requests per 'durationBounds' bucket, then those above the last bound (not cumulative)
  , statsDurationSum :: !(IORef Double)
  }

-- | Upper bounds of the request duration buckets, in seconds. A duration
-- includes sending the body, so large files land in the upper buckets.
durationBounds :: [ Double ]
durationBounds = [ 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 30, 60 ]

newStatsEnv :: IO StatsEnv
newStatsEnv = do
  start <- getMonotonicTime
  let counter = newIORef 0
  StatsEnv start <$> counter
    <*> counter
    <*> counter
    <*> counter
    <*> counter
    <*> counter
    <*> counter
    <*> counter
    <*> replicateM (length durationBounds + 1) counter
    <*> newIORef 0

-- | Requests run concurrently, so every update is atomic.
add :: Int64 -> IORef Int64 -> IO ()
add n ref = atomicModifyIORef' ref $ \old -> ( old + n, () )

count :: Members '[ Reader StatsEnv, Embed IO ] r => (StatsEnv -> IORef Int64) -> Int -> Sem r ()
count counter n = ask >>= embed . add (fromIntegral n) . counter

-- | Bytes sent to readers.
addUpload :: Members '[ Reader StatsEnv, Embed IO ] r => Int -> Sem r ()
addUpload = count statsSentBytes

-- | Bytes fetched from upstream into the cache.
addDownload :: Members '[ Reader StatsEnv, Embed IO ] r => Int -> Sem r ()
addDownload = count statsReceivedBytes

-- | One file served.
incServed :: Members '[ Reader StatsEnv, Embed IO ] r => Sem r ()
incServed = count statsSent 1

-- | One file fetched from upstream.
incFetched :: Members '[ Reader StatsEnv, Embed IO ] r => Sem r ()
incFetched = count statsReceived 1

-- | One gallery taken from the download queue.
incDlTask :: Members '[ Reader StatsEnv, Embed IO ] r => Sem r ()
incDlTask = count statsDlTasks 1

-- | One gallery file downloaded.
incDlFile :: Members '[ Reader StatsEnv, Embed IO ] r => Sem r ()
incDlFile = count statsDlFiles 1

-- | Bytes of gallery files downloaded.
addDlBytes :: Members '[ Reader StatsEnv, Embed IO ] r => Int -> Sem r ()
addDlBytes = count statsDlBytes

-- | Count a connection as active while the action runs.
trackConnection :: StatsEnv -> IO a -> IO a
trackConnection stats action = do
  add 1 (statsActiveConnections stats)
  action `finally` add (-1) (statsActiveConnections stats)

-- | Record how long a request took, in seconds.
observeDuration :: StatsEnv -> Double -> IO ()
observeDuration stats seconds = do
  whenJust (statsDurations stats !!? length (takeWhile (< seconds) durationBounds)) (add 1)
  atomicModifyIORef' (statsDurationSum stats) $ \total -> ( total + seconds, () )

-- | The metrics as they are now, for @\/metrics@.
readMetrics :: Members '[ Reader StatsEnv, Embed IO ] r => Sem r Text
readMetrics = do
  stats <- ask
  embed $ do
    now <- getMonotonicTime
    renderMetrics stats (round (now - statsStart stats))

-- | The metrics in the Prometheus text format, given the uptime in seconds.
-- They come in the order earlier versions printed them.
renderMetrics :: StatsEnv -> Int64 -> IO Text
renderMetrics stats uptime = do
  let counter name help field = scalar "counter" name help <$> readIORef (field stats)
  scalars <- sequence
    [ counter "hath_download_size_bytes_total" "Total bytes downloaded from galleries" statsDlBytes
    , counter "hath_download_file_count_total" "Total gallery files downloaded" statsDlFiles
    , counter "hath_download_task_count_total" "Total gallery download tasks" statsDlTasks
    , counter "hath_cache_received_total" "Total number of cache misses" statsReceived
    , counter "hath_cache_sent_total" "Total number of cache hits" statsSent
    , counter
        "hath_cache_received_bytes_total"
        "Total bytes received into cache"
        statsReceivedBytes
    , counter "hath_cache_sent_bytes_total" "Total bytes sent from cache" statsSentBytes
    , pure $ scalar "gauge" "hath_uptime_seconds" "Server uptime in seconds" uptime
    , scalar "gauge" "hath_active_connections" "Number of active connections"
      <$> readIORef (statsActiveConnections stats)
    ]
  durations <- traverse readIORef (statsDurations stats)
  total <- readIORef (statsDurationSum stats)
  pure $ mconcat scalars <> durationHistogram durations total

-- | A counter or gauge, with its HELP and TYPE lines.
scalar :: Text -> Text -> Text -> Int64 -> Text
scalar typ name help value
  = unlines
    [ "# HELP " <> name <> " " <> help
    , "# TYPE " <> name <> " " <> typ
    , name <> " " <> show value
    ]

-- | The request duration histogram, from the count of each bucket: cumulative
-- counts per upper bound (le is inclusive), then the sum and the count.
durationHistogram :: [ Int64 ] -> Double -> Text
durationHistogram counts total
  = unlines
  $ [ "# HELP " <> name <> " Time from request to the last byte sent"
    , "# TYPE " <> name <> " histogram"
    ]
  <> zipWith bucket (map decimal durationBounds <> [ "+Inf" ]) (scanl1 (+) counts)
  <> [ name <> "_sum " <> decimal total, name <> "_count " <> show (sum counts) ]
  where
    name        = "hath_request_duration_seconds"

    bucket le n = name <> "_bucket{le=\"" <> le <> "\"} " <> show n

    decimal x = toText (showFFloat Nothing x "")
