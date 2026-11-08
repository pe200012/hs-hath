{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import qualified Data.Map.Strict  as Map
import           Data.Text        ( Text )

import           Storage.Database ( FileRecord(..), runCachePure )

import           FileURIRenderSpec           ( fileURIRenderSpecs )
import           FilesystemSpec              ( filesystemSpecs )
import           Integration                 ( integrationSpecs )
import           InterfaceAPIOptimizationSpec ( interfaceAPIOptimizationSpecs )

import           R2Spec                      ( r2Specs )

import           Polysemy
import           Polysemy.KVStore ( KVStore, lookupKV, updateKV )

import           Relude           hiding ( Reader )

import           Test.Hspec

import           Types            ( FileURI
                                  , HathSettings(..)
                                  , defaultHathSettings
                                  , parseFileURI
                                  , parseSettings
                                  )
import qualified Metrics.Histogram as Histogram
import           Metrics.Registry  ( newRegistry, registerHistogram )
import           Metrics.Snapshot  ( generateSnapshot )

main :: IO ()
main = hspec $ do
  cacheSpecs
  interfaceAPIOptimizationSpecs
  fileURIRenderSpecs
  integrationSpecs
  r2Specs
  filesystemSpecs
  settingsSpecs
  metricsSpecs

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

-- | Helper to store a file record
storeFile :: Member (KVStore FileURI FileRecord) r => FileRecord -> Sem r ()
storeFile record = updateKV (parseFileURI $ encodeUtf8 $ fileRecordFileId record) (Just record)

-- | Helper to lookup a file by id
lookupFile :: Member (KVStore FileURI FileRecord) r => Text -> Sem r (Maybe FileRecord)
lookupFile fid = lookupKV (parseFileURI $ encodeUtf8 fid)

cacheSpecs :: Spec
cacheSpecs = describe "Cache effect" $ do
  let sampleRecord
        = FileRecord { fileRecordLRUCounter = 1
                     , fileRecordS4         = "s4-value"
                     , fileRecordFileId     = "test-id"
                     , fileRecordFileName   = Just "test.txt"
                     , fileRecordBytes      = "test content"
                     }

  describe "runCachePure" $ do
    it "should store and retrieve a file record" $ do
      let program = do
            storeFile sampleRecord
            lookupFile "test-id"
          result  = run $ runCachePure Map.empty program

      result `shouldBe` Just sampleRecord

    it "should return Nothing for non-existent file" $ do
      let program = lookupFile "non-existent"
          result  = run $ runCachePure Map.empty program

      result `shouldBe` Nothing

    it "should update existing record" $ do
      let updatedRecord = sampleRecord { fileRecordBytes = "new content" }
          program       = do
            storeFile sampleRecord
            storeFile updatedRecord
            lookupFile "test-id"
          result        = run $ runCachePure Map.empty program

      result `shouldBe` Just updatedRecord


