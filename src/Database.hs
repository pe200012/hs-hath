{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Database
    ( FileRecord(..)
    , Cache
    , initializeDB
    , runCachePure
    , runCache
    , lookupFile
    , storeFile
    ) where

import qualified Data.ByteString         as BS
import qualified Data.List               as List
import           Data.String.Interpolate ( i )

import           Database.SQLite.Simple

import           Polysemy
import           Polysemy.Reader
import           Polysemy.State

import           Relude                  hiding ( Reader, State, ask, evalState, get, modify )

data FileRecord
    = FileRecord { fileRecordLRUCounter :: {-# UNPACK #-} !Int64
                 , fileRecordS4         :: {-# UNPACK #-} !Text
                 , fileRecordFileId     :: {-# UNPACK #-} !Text
                 , fileRecordFileName   :: {-# UNPACK #-} !(Maybe Text)
                 , fileRecordBytes      :: {-# UNPACK #-} !BS.ByteString
                 }
    deriving ( Show, Generic, Eq )

instance FromRow FileRecord

instance ToRow FileRecord

data Cache m a where
    -- | Lookup file by ID
    LookupFile :: Text -> Cache m (Maybe FileRecord)
    -- | Store a new file record
    StoreFile :: FileRecord -> Cache m ()

makeSem ''Cache

-- | Initialize database with required schema
initializeDB :: Connection -> IO ()
initializeDB conn
    = execute_
        conn
        [i|CREATE TABLE IF NOT EXISTS files (
        lru_counter INTEGER NOT NULL DEFAULT 0,
        s4 TEXT NOT NULL,
        file_id TEXT PRIMARY KEY,
        file_name TEXT,
        bytes BLOB NOT NULL
    ) strict|]

-- | Run the cache with a pure state
--
-- we forget about the LRU counter and filename here
runCachePure :: [ ( Text, FileRecord ) ] -> Sem (Cache ': r) a -> Sem r a
runCachePure records
    = evalState records
    . reinterpret (\case
                       LookupFile fid   -> List.lookup fid <$> get
                       StoreFile record -> modify (( fileRecordFileId record, record ) :))

-- | Run the cache with SQLite
runCache :: Members '[ Embed IO, Reader Connection ] r => Sem (Cache ': r) a -> Sem r a
runCache = interpret $ \case
    LookupFile fid   -> do
        conn <- ask
        embed
            $ execute
                conn
                "UPDATE files SET lru_counter = lru_counter + 1 WHERE file_id = ?"
                (Only fid)
        results <- embed
            $ query
                conn
                "SELECT lru_counter, s4, file_id, file_name, bytes FROM files WHERE file_id = ?"
                (Only fid)
        return $ listToMaybe results

    StoreFile record -> do
        conn <- ask
        embed
            $ execute
                conn
                [i|INSERT OR REPLACE INTO files
                    (lru_counter, s4, file_id, file_name, bytes)
                    VALUES (?, ?, ?, ?, ?)|]
                record
