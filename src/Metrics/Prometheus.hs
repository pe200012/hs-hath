{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Metrics.Prometheus ( render ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text

import           Mason.Builder   ( BuilderFor
                                 , StrictByteStringBackend
                                 , byteString
                                 , int64Dec
                                 , intersperse
                                 , textUtf8
                                 , toStrictByteString
                                 )

import           Metrics.Types

import           Relude          hiding ( intersperse )

type MBuilder = BuilderFor StrictByteStringBackend

-- | Render a list of metric snapshots to Prometheus text format
render :: [ MetricSnapshot ] -> ByteString
render = toStrictByteString . mconcat . map renderMetric

-- | Render a single metric snapshot
renderMetric :: MetricSnapshot -> MBuilder
renderMetric = \case
  CounterSnapshot metricId help val -> renderMetricLines "counter" metricId help val
  GaugeSnapshot metricId help val   -> renderMetricLines "gauge" metricId help val

-- | Render the complete metric output (HELP, TYPE, and value lines)
renderMetricLines :: ByteString -> MetricId -> Text -> Int64 -> MBuilder
renderMetricLines typ (MetricId name labels) help val
  = mconcat
    [ "# HELP "
    , textUtf8 name
    , " "
    , textUtf8 help
    , "\n"
    , "# TYPE "
    , textUtf8 name
    , " "
    , byteString typ
    , "\n"
    , textUtf8 name
    , if Map.null labels
        then mempty
        else renderLabels labels
    , " "
    , int64Dec val
    , "\n"
    ]

-- | Render label set: {label1="value1",label2="value2"}
renderLabels :: Labels -> MBuilder
renderLabels labels
  | Map.null labels = mempty
  | otherwise = mconcat [ "{", intersperse "," $ map renderLabel $ Map.toList labels, "}" ]

-- | Render a single label: key="value"
renderLabel :: ( Text, Text ) -> MBuilder
renderLabel ( k, v ) = textUtf8 k <> "=\"" <> textUtf8 (escapeValue v) <> "\""

-- | Escape special characters in label values according to Prometheus spec
-- See: https://prometheus.io/docs/instrumenting/exposition_formats/
escapeValue :: Text -> Text
escapeValue = Text.replace "\\" "\\\\" . Text.replace "\"" "\\\"" . Text.replace "\n" "\\n"

-- Example output:
--
-- # HELP hath_cache_sent_bytes_total Total bytes sent from cache
-- # TYPE hath_cache_sent_bytes_total counter
-- hath_cache_sent_bytes_total 1234567
--
-- # HELP hath_errors_total Total errors by type
-- # TYPE hath_errors_total counter
-- hath_errors_total{type="network"} 42
-- hath_errors_total{type="timeout"} 13
--
-- Note: Labels with the same metric name but different label values
-- are currently registered as separate Counter instances.
