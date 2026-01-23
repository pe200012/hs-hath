{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Metrics.Types
  ( Labels
  , MetricId(..)
  , Counter(..)
  , Gauge(..)
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

-- | Registry holding all registered metrics
data Registry
  = Registry { registryCounters       :: !(IORef [ Counter ])
             , registryGauges         :: !(IORef [ Gauge ])
             , registryCachedSnapshot :: !(TVar ByteString)
             }

-- | Snapshot of a metric value (for rendering)
data MetricSnapshot = CounterSnapshot !MetricId !Text !Int64 | GaugeSnapshot !MetricId !Text !Int64
  deriving ( Show )
