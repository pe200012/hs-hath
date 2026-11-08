module Metrics.Registry
  ( newRegistry
  , registerCounter
  , registerGauge
  , registerCounterWith
  , registerGaugeWith
  , registerHistogram
  ) where

import           Metrics.Types

import           Relude

-- | Create a new empty registry
newRegistry :: IO Registry
newRegistry = do
  counters <- newIORef []
  gauges <- newIORef []
  histograms <- newIORef []
  cachedSnapshot <- newTVarIO ""
  pure $ Registry counters gauges histograms cachedSnapshot

-- | Register a counter without labels
registerCounter :: Text -> Text -> Registry -> IO Counter
registerCounter name help = registerCounterWith name help mempty

-- | Register a counter with labels
registerCounterWith :: Text -> Text -> Labels -> Registry -> IO Counter
registerCounterWith name help labels reg = do
  ref <- newIORef 0
  let metricId = MetricId name labels
      counter  = Counter metricId help ref
  -- Atomically prepend to counter list
  atomicModifyIORef' (registryCounters reg) $ \cs -> ( counter : cs, () )
  pure counter

-- | Register a gauge without labels
registerGauge :: Text -> Text -> Registry -> IO Gauge
registerGauge name help = registerGaugeWith name help mempty

-- | Register a gauge with labels
registerGaugeWith :: Text -> Text -> Labels -> Registry -> IO Gauge
registerGaugeWith name help labels reg = do
  ref <- newIORef 0
  let metricId = MetricId name labels
      gauge    = Gauge metricId help ref
  -- Atomically prepend to gauge list
  atomicModifyIORef' (registryGauges reg) $ \gs -> ( gauge : gs, () )
  pure gauge

-- | Register a histogram with the given ascending bucket upper bounds
registerHistogram :: Text -> Text -> [ Double ] -> Registry -> IO Histogram
registerHistogram name help bounds reg = do
  counts <- replicateM (length bounds + 1) (newIORef 0)
  total <- newIORef 0
  let histogram = Histogram (MetricId name mempty) help bounds counts total
  atomicModifyIORef' (registryHistograms reg) $ \hs -> ( histogram : hs, () )
  pure histogram

-- Note: Registration is typically done at startup, so performance is not critical.
-- Using atomicModifyIORef' ensures thread-safety if multiple threads register metrics.
--
-- Label usage example (for future):
--   errorCounter <- registerCounterWith
--     "hath_errors_total"
--     "Total errors by type"
--     (Map.fromList [("type", "network")])
--     registry
--
-- This creates a counter with fixed labels at registration time.
