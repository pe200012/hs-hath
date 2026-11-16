-- | The Prometheus text exposition format.
module Hath.Metrics.Prometheus ( render ) where

import           Data.ByteString.Builder ( Builder, byteString, int64Dec, toLazyByteString )
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as Text
import           Data.Text.Encoding      ( encodeUtf8Builder )

import           Hath.Metrics.Types

import           Numeric                 ( showFFloat )

import           Relude

-- | Render metric snapshots, each with its HELP and TYPE lines.
render :: [ MetricSnapshot ] -> ByteString
render = toStrict . toLazyByteString . foldMap renderMetric

renderMetric :: MetricSnapshot -> Builder
renderMetric = \case
  CounterSnapshot metricId help val -> renderMetricLines "counter" metricId help val
  GaugeSnapshot metricId help val   -> renderMetricLines "gauge" metricId help val
  HistogramSnapshot (MetricId name labels) help buckets total -> mconcat
    $ [ "# HELP ", text name, " ", text help, "\n" ]
    <> [ "# TYPE ", text name, " histogram\n" ]
    <> [ text name
         <> "_bucket"
         <> renderLabels (Map.insert "le" (maybe "+Inf" double le) labels)
         <> " "
         <> int64Dec n
         <> "\n"
       | ( le, n ) <- buckets
       ]
    <> [ text name, "_sum", renderLabels labels, " ", text (double total), "\n" ]
    <> [ text name
       , "_count"
       , renderLabels labels
       , " "
       , int64Dec (maybe 0 snd (viaNonEmpty last buckets))
       , "\n"
       ]
  where
    double x = toText (showFFloat Nothing x "")

-- | HELP, TYPE and value lines of a counter or gauge.
renderMetricLines :: ByteString -> MetricId -> Text -> Int64 -> Builder
renderMetricLines typ (MetricId name labels) help val
  = mconcat
    [ "# HELP "
    , text name
    , " "
    , text help
    , "\n"
    , "# TYPE "
    , text name
    , " "
    , byteString typ
    , "\n"
    , text name
    , renderLabels labels
    , " "
    , int64Dec val
    , "\n"
    ]

-- | @{label1="value1",label2="value2"}@, or nothing when there are no labels.
renderLabels :: Labels -> Builder
renderLabels labels
  | Map.null labels = mempty
  | otherwise = "{" <> mconcat (intersperse "," (map renderLabel (Map.toList labels))) <> "}"

renderLabel :: ( Text, Text ) -> Builder
renderLabel ( k, v ) = text k <> "=\"" <> text (escapeValue v) <> "\""

-- | Escape a label value as the format requires:
-- <https://prometheus.io/docs/instrumenting/exposition_formats/>
escapeValue :: Text -> Text
escapeValue = Text.replace "\\" "\\\\" . Text.replace "\"" "\\\"" . Text.replace "\n" "\\n"

text :: Text -> Builder
text = encodeUtf8Builder
