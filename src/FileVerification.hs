{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module FileVerification
  ( -- * Types
    VerificationResult(..)
  , VerificationStats(..)
    -- * Verification Operations
  , verifyFile
  , verifyAllFiles
  , verifyRandomFile
  , shouldVerifyFile
    -- * Effect
  , FileVerification(..)
  , runFileVerification
  , runFileVerificationIO
    -- * Constants
  , verificationCooldownSeconds
  , minVerificationIntervalSeconds
  ) where

import           Control.Monad         ( foldM )

import           Data.Time.Clock       ( NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime )

import           Database              ( FileRecord(..) )
import           Database.SQLite.Simple

import           Hash                  ( hash )

import           Polysemy
import           Polysemy.Operators

import           Relude

import           Types                 ( FileURI(..), parseFileURI )

-- | Result of verifying a single file
data VerificationResult
  = VerificationOK
  | VerificationCorrupted {-# UNPACK #-} !Text  -- ^ File ID of corrupted file
  | VerificationMissing {-# UNPACK #-} !Text    -- ^ File ID that should exist but doesn't
  deriving ( Show, Eq, Generic )

-- | Statistics from a verification run
data VerificationStats
  = VerificationStats
  { verifiedCount   :: {-# UNPACK #-} !Int
  , corruptedCount  :: {-# UNPACK #-} !Int
  , missingCount    :: {-# UNPACK #-} !Int
  , deletedCount    :: {-# UNPACK #-} !Int
  , verificationTime :: {-# UNPACK #-} !NominalDiffTime
  }
  deriving ( Show, Eq, Generic )

instance Semigroup VerificationStats where
  a <> b = VerificationStats
    { verifiedCount   = verifiedCount a + verifiedCount b
    , corruptedCount  = corruptedCount a + corruptedCount b
    , missingCount    = missingCount a + missingCount b
    , deletedCount    = deletedCount a + deletedCount b
    , verificationTime = verificationTime a + verificationTime b
    }

instance Monoid VerificationStats where
  mempty = VerificationStats 0 0 0 0 0

-- | Cooldown between verifying the same file (1 week in seconds)
verificationCooldownSeconds :: Int64
verificationCooldownSeconds = 7 * 24 * 60 * 60

-- | Minimum interval between any verification checks (2 seconds)
minVerificationIntervalSeconds :: NominalDiffTime
minVerificationIntervalSeconds = 2

-- | Check if a file should be verified based on last verification time
shouldVerifyFile :: UTCTime -> UTCTime -> Bool
shouldVerifyFile lastVerified now =
  diffUTCTime now lastVerified > fromIntegral verificationCooldownSeconds

-- | Verify a single file's integrity by comparing stored hash with computed hash
verifyFile :: FileRecord -> VerificationResult
verifyFile record =
  let storedFileId = fileRecordFileId record
      content      = fileRecordBytes record
      uri          = parseFileURI (encodeUtf8 storedFileId)
      -- Extract expected hash from FileURI (already ByteString)
      -- FileURI format: hash-size-xres-yres-ext
      expectedHash = fileHash uri
      computedHash = hash @ByteString content
  in if expectedHash == computedHash
     then VerificationOK
     else VerificationCorrupted storedFileId

-- | Effect for file verification operations
data FileVerification m a where
  -- | Verify all files in the cache (for startup)
  VerifyAllFilesE :: FileVerification m VerificationStats
  -- | Verify a single random file (for periodic checks)
  VerifyRandomFileE :: FileVerification m (Maybe VerificationResult)
  -- | Get the last verification time
  GetLastVerificationTime :: FileVerification m (Maybe UTCTime)
  -- | Update the last verification time
  SetLastVerificationTime :: UTCTime -> FileVerification m ()

makeSem ''FileVerification

-- | Run file verification with SQLite connection
runFileVerification :: Member (Embed IO) r
                    => Connection
                    -> TVar (Maybe UTCTime)  -- ^ Last verification time
                    -> FileVerification : r @> a
                    -> r @> a
runFileVerification conn lastVerifVar = interpret $ \case
  VerifyAllFilesE -> embed @IO $ verifyAllFiles conn

  VerifyRandomFileE -> embed @IO $ verifyRandomFile conn lastVerifVar

  GetLastVerificationTime -> embed @IO $ readTVarIO lastVerifVar

  SetLastVerificationTime t -> embed @IO $ atomically $ writeTVar lastVerifVar (Just t)

-- | Verify all files in the database
verifyAllFiles :: Connection -> IO VerificationStats
verifyAllFiles conn = do
  startTime <- getCurrentTime
  -- Get all files from database
  records <- query_ conn
    "SELECT lru_counter, s4, file_id, file_name, bytes FROM files" :: IO [FileRecord]

  -- Verify each file and collect results
  ( stats, toDelete ) <- foldM verifyAndCollect ( mempty, [] ) records

  -- Delete corrupted files
  forM_ toDelete $ \fileId ->
    execute conn "DELETE FROM files WHERE file_id = ?" (Only fileId)

  endTime <- getCurrentTime
  let elapsed = diffUTCTime endTime startTime

  pure stats
    { deletedCount = length toDelete
    , verificationTime = elapsed
    }
  where
    verifyAndCollect ( stats, toDelete ) record =
      case verifyFile record of
        VerificationOK ->
          pure ( stats { verifiedCount = verifiedCount stats + 1 }, toDelete )
        VerificationCorrupted fileId ->
          pure ( stats { corruptedCount = corruptedCount stats + 1 }
               , fileId : toDelete
               )
        VerificationMissing fileId ->
          pure ( stats { missingCount = missingCount stats + 1 }
               , fileId : toDelete
               )

-- | Verify a random file from the cache
-- Returns Nothing if verification is on cooldown or no files exist
verifyRandomFile :: Connection -> TVar (Maybe UTCTime) -> IO (Maybe VerificationResult)
verifyRandomFile conn lastVerifVar = do
  now <- getCurrentTime
  lastVerif <- readTVarIO lastVerifVar

  -- Check cooldown
  case lastVerif of
    Just t | diffUTCTime now t < minVerificationIntervalSeconds ->
      pure Nothing  -- On cooldown
    _ -> do
      -- Get a random file that hasn't been verified recently
      -- We use RANDOM() for simplicity; production might want better randomization
      maybeRecord <- listToMaybe <$> query_ conn
        "SELECT lru_counter, s4, file_id, file_name, bytes FROM files ORDER BY RANDOM() LIMIT 1"

      case maybeRecord of
        Nothing -> pure Nothing  -- No files in cache
        Just record -> do
          -- Update last verification time
          atomically $ writeTVar lastVerifVar (Just now)

          let result = verifyFile record

          -- If corrupted, delete the file
          case result of
            VerificationCorrupted fileId -> do
              execute conn "DELETE FROM files WHERE file_id = ?" (Only fileId)
              pure $ Just result
            _ -> pure $ Just result

-- | Run file verification in IO
runFileVerificationIO :: Connection
                      -> TVar (Maybe UTCTime)
                      -> [FileVerification, Embed IO, Final IO] @> a
                      -> IO a
runFileVerificationIO conn lastVerifVar =
  runFinal . embedToFinal . runFileVerification conn lastVerifVar
