{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import qualified Data.Map.Strict  as Map
import           Data.Text        ( Text )

import           Database

import           Integration      ( integrationSpecs )

import           R2Spec           ( r2Specs )

import           Polysemy
import           Polysemy.KVStore ( KVStore, lookupKV, updateKV )

import           Relude           hiding ( Reader )

import           Test.Hspec

import           Types            ( FileURI, parseFileURI )

main :: IO ()
main = hspec $ do
  cacheSpecs
  integrationSpecs
  r2Specs

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


