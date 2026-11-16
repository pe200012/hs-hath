import           FileURISpec           ( fileURISpecs )

import           FilesystemSpec        ( filesystemSpecs )

import           FloodControlSpec      ( floodControlSpecs )

import           GallerySpec           ( gallerySpecs )

import qualified Hath.Metrics.Histogram as Histogram
import           Hath.Metrics.Registry ( newRegistry, registerHistogram )
import           Hath.Metrics.Snapshot ( generateSnapshot )
import           Hath.Settings         ( HathSettings(..), defaultHathSettings, parseSettings )

import           Integration           ( integrationSpecs )

import           ProtocolSpec          ( protocolSpecs )

import           R2Spec                ( r2Specs )

import           Relude

import           Test.Hspec

main :: IO ()
main = hspec $ do
  fileURISpecs
  protocolSpecs
  floodControlSpecs
  gallerySpecs
  settingsSpecs
  metricsSpecs
  integrationSpecs
  r2Specs
  filesystemSpecs

settingsSpecs :: Spec
settingsSpecs = describe "parseSettings" $
  it "keeps what a refresh does not mention" $ do
    let login     = parseSettings defaultHathSettings
          [ "port=443", "disklimit_bytes=1000", "rpc_server_ip=192.0.2.1;192.0.2.2" ]
        refreshed = parseSettings login [ "disklimit_bytes=2000" ]
    clientPort refreshed `shouldBe` 443
    diskLimitBytes refreshed `shouldBe` 2000
    rpcServerIPs refreshed `shouldBe` [ "192.0.2.1", "192.0.2.2" ]

metricsSpecs :: Spec
metricsSpecs = describe "histogram metrics" $
  it "renders cumulative buckets (le is inclusive), sum and count" $ do
    reg <- newRegistry
    h <- registerHistogram "t_seconds" "test" [ 0.5, 1 ] reg
    mapM_ (`Histogram.observe` h) [ 0.25, 0.5, 4 ]
    generateSnapshot reg >>= (`shouldBe` mconcat
      [ "# HELP t_seconds test\n"
      , "# TYPE t_seconds histogram\n"
      , "t_seconds_bucket{le=\"0.5\"} 2\n"
      , "t_seconds_bucket{le=\"1.0\"} 2\n"
      , "t_seconds_bucket{le=\"+Inf\"} 3\n"
      , "t_seconds_sum 4.75\n"
      , "t_seconds_count 3\n"
      ])
