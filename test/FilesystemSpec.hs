{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module FilesystemSpec ( filesystemSpecs ) where

import           Colog                ( LogAction(..), Message )
import           Colog.Polysemy       ( Log, runLogAction )

import           Storage.Database     ( FileRecord(..) )

import           Polysemy
import           Polysemy.Error       ( Error, runError )
import           Polysemy.KVStore     ( KVStore, lookupKV, updateKV )

import           Storage.Filesystem   ( runCacheFilesystem )

import           Relude               hiding ( Reader )

import           System.Directory     ( doesFileExist )
import           System.FilePath      ( (</>) )
import           System.IO.Temp       ( withSystemTempDirectory )

import           Test.Hspec

import           Types                ( FileURI, RPCError, parseFileURI )

-- | Sample file record for testing
sampleRecord :: FileRecord
sampleRecord = FileRecord
  {
    fileRecordLRUCounter = 0
  ,
    fileRecordS4         = "test"
  ,
    fileRecordFileId     = "testhash1234-1024-800-600-jpg"
  ,
    fileRecordFileName   = Just "test.jpg"
  ,
    fileRecordBytes      = "test content for Filesystem storage"
  }

-- | Parse a file URI from the sample record
sampleURI :: FileURI
sampleURI = parseFileURI "testhash1234-1024-800-600-jpg"

-- | Silence logs for cleaner test output
silentLogAction :: LogAction IO Message
silentLogAction = LogAction $ \_ -> pure ()

-- | Run a program with Filesystem cache effect (silent logging)
runWithFilesystemSilent :: FilePath
                        -> Sem '[KVStore FileURI FileRecord, Log Message, Error RPCError, Embed IO] a 
                        -> IO (Either RPCError a)
runWithFilesystemSilent root program = 
  runM
  . runError @RPCError
  . runLogAction @IO @Message silentLogAction
  . runCacheFilesystem root
  $ program

-- | Lookup with explicit type
lookupFile :: Member (KVStore FileURI FileRecord) r => FileURI -> Sem r (Maybe FileRecord)
lookupFile = lookupKV

-- | Update with explicit type
updateFile :: Member (KVStore FileURI FileRecord) r => FileURI -> Maybe FileRecord -> Sem r ()
updateFile = updateKV

filesystemSpecs :: Spec
filesystemSpecs = describe "Filesystem Cache Effect" $ do
  
  it "should store and retrieve a file record" $ do
    withSystemTempDirectory "hath-test" $ \tempDir -> do
      let root = tempDir </> "cache"
      
      -- Store
      storeResult <- runWithFilesystemSilent root $ updateFile sampleURI (Just sampleRecord)
      storeResult `shouldBe` Right ()
      
      -- Verify file exists on disk (impl detail check)
      -- Path: root/s4/fileid
      -- s4 for sampleURI: "test" (first 4 of hash? No, show uri)
      -- show uri: testhash1234-1024-800-600-jpg
      -- s4: test
      let expectedPath = root </> "test" </> "testhash1234-1024-800-600-jpg"
      exists <- doesFileExist expectedPath
      exists `shouldBe` True

      -- Retrieve
      retrieveResult <- runWithFilesystemSilent root $ lookupFile sampleURI
      case retrieveResult of
        Left err -> expectationFailure $ "Error: " <> show err
        Right Nothing -> expectationFailure "Expected file record but got Nothing"
        Right (Just record) -> do
          fileRecordBytes record `shouldBe` fileRecordBytes sampleRecord

  it "should return Nothing for non-existent file" $ do
    withSystemTempDirectory "hath-test" $ \tempDir -> do
      let root = tempDir </> "cache"
      let nonExistentURI = parseFileURI "nonexistent999-1024-800-600-jpg"
      
      result <- runWithFilesystemSilent root $ lookupFile nonExistentURI
      case result of
        Left err -> expectationFailure $ "Error: " <> show err
        Right Nothing -> pure ()
        Right (Just _) -> expectationFailure "Expected Nothing but got a record"

  it "should delete a file record" $ do
    withSystemTempDirectory "hath-test" $ \tempDir -> do
      let root = tempDir </> "cache"
      
      -- Store first
      _ <- runWithFilesystemSilent root $ updateFile sampleURI (Just sampleRecord)
      
      -- Delete
      deleteResult <- runWithFilesystemSilent root $ updateFile sampleURI Nothing
      deleteResult `shouldBe` Right ()
      
      -- Verify deleted on disk
      let expectedPath = root </> "test" </> "testhash1234-1024-800-600-jpg"
      exists <- doesFileExist expectedPath
      exists `shouldBe` False
      
      -- Verify lookup returns Nothing
      lookupResult <- runWithFilesystemSilent root $ lookupFile sampleURI
      lookupResult `shouldBe` Right Nothing
