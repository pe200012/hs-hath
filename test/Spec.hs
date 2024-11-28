{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import           Data.Text       ( Text )

import           Database

import           Polysemy

import           Relude

import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Cache effect" $ do
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
                    result  = run $ runCachePure [] program

                result `shouldBe` Just sampleRecord

            it "should return Nothing for non-existent file" $ do
                let program = lookupFile "non-existent"
                    result  = run $ runCachePure [] program

                result `shouldBe` Nothing

            it "should update existing record" $ do
                let updatedRecord = sampleRecord { fileRecordBytes = "new content" }
                    program       = do
                        storeFile sampleRecord
                        storeFile updatedRecord
                        lookupFile "test-id"
                    result        = run $ runCachePure [] program

                result `shouldBe` Just updatedRecord
