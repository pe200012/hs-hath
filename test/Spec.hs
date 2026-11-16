import           Dhall            ( embed, inject )
import qualified Dhall.Core       as Dhall

import           FileURISpec      ( fileURISpecs )

import           FilesystemSpec   ( filesystemSpecs )

import           FloodControlSpec ( floodControlSpecs )

import           GallerySpec      ( gallerySpecs )

import           Hath.Config      ( CacheBackend(..)
                                  , ClientConfig(..)
                                  , R2Config(..)
                                  , defaultClientConfig
                                  , readClientConfig
                                  )
import           Hath.Settings    ( HathSettings(..), defaultHathSettings, parseSettings )
import           Hath.Stats       ( addDlBytes
                                  , addDownload
                                  , addUpload
                                  , incDlFile
                                  , incDlTask
                                  , incFetched
                                  , incServed
                                  , newStatsEnv
                                  , observeDuration
                                  , renderMetrics
                                  , trackConnection
                                  )

import           Integration      ( integrationSpecs )

import           Polysemy         ( runM )
import           Polysemy.Reader  ( runReader )

import           ProtocolSpec     ( protocolSpecs )

import           R2Spec           ( r2Specs )

import           Relude           hiding ( runReader )

import           Test.Hspec

main :: IO ()
main = hspec $ do
  fileURISpecs
  protocolSpecs
  floodControlSpecs
  gallerySpecs
  settingsSpecs
  configSpecs
  metricsSpecs
  integrationSpecs
  r2Specs
  filesystemSpecs

settingsSpecs :: Spec
settingsSpecs = describe "parseSettings" $ it "keeps what a refresh does not mention" $ do
  let login
        = parseSettings
          defaultHathSettings
          [ "port=443", "disklimit_bytes=1000", "rpc_server_ip=192.0.2.1;192.0.2.2" ]
      refreshed = parseSettings login [ "disklimit_bytes=2000" ]
  clientPort refreshed `shouldBe` 443
  diskLimitBytes refreshed `shouldBe` 2000
  rpcServerIPs refreshed `shouldBe` [ "192.0.2.1", "192.0.2.2" ]

-- | hath-bench writes its config this way.
configSpecs :: Spec
configSpecs = describe "ClientConfig" $ it "reads back what ToDhall writes" $ do
  let config
        = defaultClientConfig
        { clientId     = "12345"
        , key          = "a \"quoted\" key"
        , cachePath    = "/tmp/hs-cache.db"
        , cacheBackend = CacheBackendR2
        , r2Config     = Just (R2Config "https://r2.example" "bucket")
        }
  readBack <- readClientConfig (Dhall.pretty (embed inject config))
  show @Text readBack `shouldBe` show config

metricsSpecs :: Spec
metricsSpecs
  = describe "/metrics"
  $ it "renders every metric: names, HELP and TYPE lines, order, numbers, buckets"
  $ do
    stats <- newStatsEnv
    runM $ runReader stats $ do
      addUpload 1000
      addUpload 234
      addDownload 5678
      replicateM_ 3 incServed
      replicateM_ 2 incFetched
      incDlTask
      replicateM_ 4 incDlFile
      addDlBytes 987654321
    -- on a bound (le is inclusive), between bounds, past the last one
    mapM_ (observeDuration stats) [ 0.005, 0.3, 2.5, 75, 0.0001 ]
    -- one connection has come and gone, two are open
    trackConnection stats pass
    rendered <- trackConnection stats $ trackConnection stats $ renderMetrics stats 42
    rendered `shouldBe` golden
  where
    golden
      = unlines
        [ "# HELP hath_download_size_bytes_total Total bytes downloaded from galleries"
        , "# TYPE hath_download_size_bytes_total counter"
        , "hath_download_size_bytes_total 987654321"
        , "# HELP hath_download_file_count_total Total gallery files downloaded"
        , "# TYPE hath_download_file_count_total counter"
        , "hath_download_file_count_total 4"
        , "# HELP hath_download_task_count_total Total gallery download tasks"
        , "# TYPE hath_download_task_count_total counter"
        , "hath_download_task_count_total 1"
        , "# HELP hath_cache_received_total Total number of cache misses"
        , "# TYPE hath_cache_received_total counter"
        , "hath_cache_received_total 2"
        , "# HELP hath_cache_sent_total Total number of cache hits"
        , "# TYPE hath_cache_sent_total counter"
        , "hath_cache_sent_total 3"
        , "# HELP hath_cache_received_bytes_total Total bytes received into cache"
        , "# TYPE hath_cache_received_bytes_total counter"
        , "hath_cache_received_bytes_total 5678"
        , "# HELP hath_cache_sent_bytes_total Total bytes sent from cache"
        , "# TYPE hath_cache_sent_bytes_total counter"
        , "hath_cache_sent_bytes_total 1234"
        , "# HELP hath_uptime_seconds Server uptime in seconds"
        , "# TYPE hath_uptime_seconds gauge"
        , "hath_uptime_seconds 42"
        , "# HELP hath_active_connections Number of active connections"
        , "# TYPE hath_active_connections gauge"
        , "hath_active_connections 2"
        , "# HELP hath_request_duration_seconds Time from request to the last byte sent"
        , "# TYPE hath_request_duration_seconds histogram"
        , "hath_request_duration_seconds_bucket{le=\"0.005\"} 2"
        , "hath_request_duration_seconds_bucket{le=\"0.01\"} 2"
        , "hath_request_duration_seconds_bucket{le=\"0.025\"} 2"
        , "hath_request_duration_seconds_bucket{le=\"0.05\"} 2"
        , "hath_request_duration_seconds_bucket{le=\"0.1\"} 2"
        , "hath_request_duration_seconds_bucket{le=\"0.25\"} 2"
        , "hath_request_duration_seconds_bucket{le=\"0.5\"} 3"
        , "hath_request_duration_seconds_bucket{le=\"1.0\"} 3"
        , "hath_request_duration_seconds_bucket{le=\"2.5\"} 4"
        , "hath_request_duration_seconds_bucket{le=\"5.0\"} 4"
        , "hath_request_duration_seconds_bucket{le=\"10.0\"} 4"
        , "hath_request_duration_seconds_bucket{le=\"30.0\"} 4"
        , "hath_request_duration_seconds_bucket{le=\"60.0\"} 4"
        , "hath_request_duration_seconds_bucket{le=\"+Inf\"} 5"
        , "hath_request_duration_seconds_sum 77.80510000000001"
        , "hath_request_duration_seconds_count 5"
        ]
