{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Metrics.Types
  ( Labels
  , MetricId(..)
  , Counter(..)
  , Gauge(..)
  , Histogram(..)
  , Registry(..)
  , MetricSnapshot(..)
  ) where

import           GHC.Generics

import           Relude

-- | Label support for metrics (key-value pairs)
type Labels = Map Text Text

-- | Metric identifier combining name and labels
data MetricId = MetricId { metricName :: !Text, metricLabels :: !Labels }
  deriving ( Eq, Ord, Show, Generic )

-- | Counter metric (monotonically increasing)
data Counter
  = Counter { counterMetricId :: !MetricId, counterHelp :: !Text, counterRef :: !(IORef Int64) }

-- | Gauge metric (can increase or decrease)
data Gauge = Gauge { gaugeMetricId :: !MetricId, gaugeHelp :: !Text, gaugeRef :: !(IORef Int64) }

-- | Histogram metric: observation counts per upper bound, plus +Inf, and their sum
data Histogram
  = Histogram { histogramMetricId :: !MetricId
              , histogramHelp     :: !Text
              , histogramBounds   :: ![ Double ]       -- ^ ascending upper bounds
              , histogramCounts   :: ![ IORef Int64 ]  -- ^ one per bound, then +Inf (not cumulative)
              , histogramSum      :: !(IORef Double)
              }

-- | Registry holding all registered metrics
data Registry
  = Registry { registryCounters       :: !(IORef [ Counter ])
             , registryGauges         :: !(IORef [ Gauge ])
             , registryHistograms     :: !(IORef [ Histogram ])
             , registryCachedSnapshot :: !(TVar ByteString)
             }

-- | Snapshot of a metric value (for rendering)
data MetricSnapshot
  = CounterSnapshot !MetricId !Text !Int64
  | GaugeSnapshot !MetricId !Text !Int64
    -- | cumulative count per upper bound (Nothing is +Inf), and the sum
  | HistogramSnapshot !MetricId !Text ![ ( Maybe Double, Int64 ) ] !Double
  deriving ( Show )
