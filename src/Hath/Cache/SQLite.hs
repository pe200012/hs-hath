{-# LANGUAGE DataKinds #-}

-- | The SQLite backend: files are blobs in one database file. Nothing is ever
-- evicted; kept for existing setups.
module Hath.Cache.SQLite
  ( initializeDB
  , runCacheSQLite
    -- * Startup verification
  , VerificationStats(..)
  , verifyAllFiles
  ) where

import qualified Data.Text              as T
import           Data.Time.Clock        ( NominalDiffTime, diffUTCTime, getCurrentTime )

import           Database.SQLite.Simple

import           Hath.Cache             ( Cache(..), CacheHit(..) )
import           Hath.FileURI           ( FileURI(..), parseFileURI, renderFileURIText )
import           Hath.Protocol          ( sha1Hex )

import           Polysemy

import           Relude

-- | Create the table if it is missing and set the pragmas we run with.
initializeDB :: Connection -> IO ()
initializeDB conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS files (\
    \  lru_counter INTEGER NOT NULL DEFAULT 0,\
    \  s4 TEXT NOT NULL,\
    \  file_id TEXT PRIMARY KEY,\
    \  file_name TEXT,\
    \  bytes BLOB NOT NULL\
    \) strict"
  execute_ conn "pragma journal_mode=delete"
  execute_ conn "pragma synchronous=normal"
  execute_ conn "pragma temp_store=memory"

runCacheSQLite :: Member (Embed IO) r => Connection -> Sem (Cache : r) a -> Sem r a
runCacheSQLite conn = interpret $ \case
  LookupFile uri      -> embed $ do
    rows <- query conn "SELECT bytes FROM files WHERE file_id = ?" (Only (renderFileURIText uri))
    pure $ InMemory . fromOnly <$> listToMaybe rows

  StoreFile uri bytes -> embed $ do
    let fileId = renderFileURIText uri
    execute
      conn
      "INSERT OR REPLACE INTO files (lru_counter, s4, file_id, file_name, bytes) VALUES (?, ?, ?, ?, ?)"
      ( 1 :: Int64, T.take 4 fileId, fileId, Nothing :: Maybe Text, bytes )

data VerificationStats
  = VerificationStats { verifiedCount    :: !Int
                      , corruptedCount   :: !Int
                        -- ^ these were deleted
                      , verificationTime :: !NominalDiffTime
                      }
  deriving ( Show, Eq )

-- | Whether the content still has the SHA-1 its file id promises.
intact :: Text -> ByteString -> Bool
intact fileId bytes = (fileHash <$> parseFileURI (encodeUtf8 fileId)) == Just (sha1Hex bytes)

-- | Check every file against its id and delete the ones that no longer match.
-- Rows are streamed, so the cache is never in memory as a whole.
verifyAllFiles :: Connection -> IO VerificationStats
verifyAllFiles conn = do
  start <- getCurrentTime
  ( verified, corrupted ) <- fold_ conn "SELECT file_id, bytes FROM files" ( 0, [] )
    $ \( !ok, bad ) ( fileId, bytes ) -> pure
    $ if intact fileId bytes
      then ( ok + 1, bad )
      else ( ok, fileId : bad )
  forM_ corrupted $ \fileId -> execute conn "DELETE FROM files WHERE file_id = ?" (Only fileId)
  end <- getCurrentTime
  pure
    VerificationStats { verifiedCount    = verified
                      , corruptedCount   = length corrupted
                      , verificationTime = diffUTCTime end start
                      }
