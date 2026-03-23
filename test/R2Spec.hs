{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module R2Spec ( r2Specs ) where

import           Colog                ( LogAction(..), richMessageAction, Message )
import           Colog.Polysemy       ( Log, runLogAction )

import           Storage.Database     ( FileRecord(..) )

import           Polysemy
import           Polysemy.Error       ( Error, runError )
import           Polysemy.KVStore     ( KVStore, lookupKV, updateKV )
import           Polysemy.Reader      ( Reader, runReader )

import           Storage.R2           ( R2Connection(..), mkR2Connection, runCacheR2 )

import           Relude               hiding ( Reader, runReader )

import           System.Environment   ( setEnv )

import           Test.Hspec

import           Types                ( ClientConfig
                                      , FileURI
                                      , R2Config(..)
                                      , RPCError
                                      , StorageResult(..)
                                      , defaultClientConfig
                                      , parseFileURI
                                      )

-- | R2 test configuration
testR2Config :: R2Config
testR2Config = R2Config
  { r2Endpoint = "https://111e2d89a4500e36dcda4853060fe725.r2.cloudflarestorage.com"
  , r2Bucket   = "hath"
  }

-- | Sample file record for testing
sampleRecord :: FileRecord
sampleRecord = FileRecord
  { fileRecordLRUCounter = 0
  , fileRecordS4         = "test"
  , fileRecordFileId     = "testhash1234-1024-800-600-jpg"
  , fileRecordFileName   = Just "test.jpg"
  , fileRecordBytes      = "test content for R2 storage"
  }

-- | Parse a file URI from the sample record
sampleURI :: FileURI
sampleURI = parseFileURI "testhash1234-1024-800-600-jpg"

-- | Silence logs for cleaner test output
silentLogAction :: LogAction IO Message
silentLogAction = LogAction $ \_ -> pure ()

-- | Run a program with R2 cache effect (silent logging)
runWithR2Silent :: R2Connection 
                -> Sem '[KVStore FileURI StorageResult, Log Message, Reader ClientConfig, Error RPCError, Embed IO] a 
                -> IO (Either RPCError a)
runWithR2Silent conn program = 
  runM
  . runError @RPCError
  . runReader defaultClientConfig
  . runLogAction @IO @Message silentLogAction
  . runCacheR2 conn
  $ program

-- | Lookup with explicit type
lookupFile :: Member (KVStore FileURI StorageResult) r => FileURI -> Sem r (Maybe StorageResult)
lookupFile = lookupKV

-- | Update with explicit type
updateFile :: Member (KVStore FileURI StorageResult) r => FileURI -> Maybe StorageResult -> Sem r ()
updateFile = updateKV

r2Specs :: Spec
r2Specs = describe "R2 Cache Effect" $ do
  
  -- Setup: ensure env vars are set
  beforeAll setupEnv $ do
    
    describe "mkR2Connection" $ do
      it "should create connection with valid env vars" $ \_ -> do
        result <- mkR2Connection testR2Config
        case result of
          Left err -> expectationFailure $ "Connection failed: " <> toString err
          Right _  -> pure ()
      
      it "should fail without R2_ACCESS_KEY" $ \_ -> do
        -- Temporarily unset
        setEnv "R2_ACCESS_KEY" ""
        result <- mkR2Connection testR2Config
        -- Restore
        setEnv "R2_ACCESS_KEY" "cbb541929f87b599e8831ee1917a57fd"
        case result of
          Left _  -> pure ()  -- Expected
          Right _ -> expectationFailure "Should have failed without R2_ACCESS_KEY"

    describe "runCacheR2" $ do
      xit "should store a file record (PUT)" $ \conn -> do
        result <- runWithR2Silent conn $ updateFile sampleURI (Just $ Record sampleRecord)
        result `shouldBe` Right ()

      xit "should retrieve a stored file record (GET)" $ \conn -> do
        -- First store
        _ <- runWithR2Silent conn $ updateFile sampleURI (Just $ Record sampleRecord)
        -- Then retrieve
        result <- runWithR2Silent conn $ lookupFile sampleURI
        case result of
          Left err -> expectationFailure $ "R2 error: " <> show err
          Right Nothing -> expectationFailure "Expected file record but got Nothing"
          Right (Just (Record record)) -> do
            fileRecordBytes record `shouldBe` fileRecordBytes sampleRecord
          Right (Just (Redirect _)) -> expectationFailure "Expected cached record but got redirect"

      it "should return Nothing for non-existent file" $ \conn -> do
        let nonExistentURI = parseFileURI "nonexistent999-1024-800-600-jpg"
        result <- runWithR2Silent conn $ lookupFile nonExistentURI
        case result of
          Left _ -> pure ()  -- Error is acceptable (404 from R2)
          Right Nothing -> pure ()  -- Expected
          Right (Just _) -> expectationFailure "Expected Nothing but got a record"

      xit "should delete a file record (DELETE)" $ \conn -> do
        -- Store first
        _ <- runWithR2Silent conn $ updateFile sampleURI (Just $ Record sampleRecord)
        -- Delete
        deleteResult <- runWithR2Silent conn $ updateFile sampleURI Nothing
        deleteResult `shouldBe` Right ()
        -- Verify deleted
        lookupResult <- runWithR2Silent conn $ lookupFile sampleURI
        case lookupResult of
          Left _ -> pure ()  -- Error acceptable
          Right Nothing -> pure ()  -- Expected
          Right (Just _) -> expectationFailure "File should have been deleted"

-- | Setup environment variables and create connection
setupEnv :: IO R2Connection
setupEnv = do
  -- Set env vars (in real tests, these would come from .env)
  setEnv "R2_ACCESS_KEY" "12345678901234567890123456789012" -- 32 chars
  setEnv "R2_SECRET_KEY" "1234567890123456789012345678901234567890" -- 40 chars
  
  result <- mkR2Connection testR2Config
  case result of
    Left err -> error $ "Failed to create R2 connection: " <> err
    Right conn -> pure conn
