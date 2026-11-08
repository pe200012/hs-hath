{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module FileVerification
  ( VerificationResult(..)
  , VerificationStats(..)
  , verifyFile
  , verifyAllFiles
  ) where

import           Data.Time.Clock        ( NominalDiffTime, diffUTCTime, getCurrentTime )

import           Database.SQLite.Simple

import           Relude

import           Storage.Database       ( FileRecord(..) )

import           Types                  ( FileURI(..), parseFileURI )

import           Utils                  ( hash )

-- | Result of verifying a single file
data VerificationResult
  = VerificationOK
  | VerificationCorrupted {-# UNPACK #-} !Text  -- ^ File ID of corrupted file
  deriving ( Show, Eq, Generic )

-- | Statistics from a verification run
data VerificationStats
  = VerificationStats { verifiedCount    :: {-# UNPACK #-} !Int
                      , corruptedCount   :: {-# UNPACK #-} !Int
                      , deletedCount     :: {-# UNPACK #-} !Int
                      , verificationTime :: {-# UNPACK #-} !NominalDiffTime
                      }
  deriving ( Show, Eq, Generic )

-- | Verify a single file's integrity by comparing the hash in its id with its content
verifyFile :: FileRecord -> VerificationResult
verifyFile record
  = if fileHash (parseFileURI (encodeUtf8 storedFileId)) == hash @ByteString (fileRecordBytes record)
    then VerificationOK
    else VerificationCorrupted storedFileId
  where
    storedFileId = fileRecordFileId record

-- | Verify all files in the database using streaming to avoid loading all into memory
verifyAllFiles :: Connection -> IO VerificationStats
verifyAllFiles conn = do
  startTime <- getCurrentTime
  statsRef <- newIORef $! VerificationStats 0 0 0 0
  toDeleteRef <- newIORef ([] :: [ Text ])

  fold_ conn "SELECT lru_counter, s4, file_id, file_name, bytes FROM files" ()
    $ \() record -> case verifyFile record of
      VerificationOK -> modifyIORef' statsRef $ \s -> s { verifiedCount = verifiedCount s + 1 }
      VerificationCorrupted fileId -> do
        modifyIORef' statsRef $ \s -> s { corruptedCount = corruptedCount s + 1 }
        modifyIORef' toDeleteRef (fileId :)

  toDelete <- readIORef toDeleteRef
  forM_ toDelete $ \fileId -> execute conn "DELETE FROM files WHERE file_id = ?" (Only fileId)

  endTime <- getCurrentTime
  stats <- readIORef statsRef
  pure
    $! stats { deletedCount = length toDelete, verificationTime = diffUTCTime endTime startTime }
