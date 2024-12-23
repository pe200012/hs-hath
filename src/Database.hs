{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Database ( FileRecord(..), initializeDB, runCache, runCachePure ) where

import qualified Data.ByteString         as BS
import           Data.String.Interpolate ( i )

import           Database.SQLite.Simple

import           Polysemy
import           Polysemy.KVStore        ( KVStore(..), runKVStorePurely )
import           Polysemy.Operators

import           Relude                  hiding ( Reader, State, ask, evalState, get, modify, put )

import           Types                   ( FileURI )

data FileRecord
    = FileRecord { fileRecordLRUCounter :: {-# UNPACK #-} !Int64
                 , fileRecordS4         :: {-# UNPACK #-} !Text
                 , fileRecordFileId     :: {-# UNPACK #-} !Text
                 , fileRecordFileName   :: {-# UNPACK #-} !(Maybe Text)
                 , fileRecordBytes      :: !BS.ByteString
                 }
    deriving ( Show, Generic, Eq )

instance FromRow FileRecord

instance ToRow FileRecord

{-# INLINE initializeDB #-}
-- | Initialize database with required schema
initializeDB :: Connection -> IO ()
initializeDB conn = do
    execute_
        conn
        [i|CREATE TABLE IF NOT EXISTS files (
        lru_counter INTEGER NOT NULL DEFAULT 0,
        s4 TEXT NOT NULL,
        file_id TEXT PRIMARY KEY,
        file_name TEXT,
        bytes BLOB NOT NULL
    ) strict|]
    execute_ conn "pragma journal_mode=WAL"
    execute_ conn "pragma synchronous=normal"
    execute_ conn "pragma temp_store=memory"
    execute_ conn "pragma cache_size=100000"
    execute_ conn "pragma mmap_size=65536"

-- | Run the cache with SQLite
runCache
    :: Members '[ Embed IO ] r => Connection -> Sem (KVStore FileURI FileRecord ': r) a -> Sem r a
runCache conn = interpret $ \case
    LookupKV uri -> do
        let fid = show @Text uri
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

    UpdateKV _ (Just record) -> do
        embed
            $ execute
                conn
                [i|INSERT OR REPLACE INTO files
                    (lru_counter, s4, file_id, file_name, bytes)
                    VALUES (?, ?, ?, ?, ?)|]
                record

    UpdateKV uri Nothing -> let
        fid = show @ByteString uri
        in 
            embed $ execute conn "DELETE FROM files WHERE file_id = ?" (Only fid)

{-# INLINE runCachePure #-}
runCachePure :: Map FileURI FileRecord -> KVStore FileURI FileRecord : r @> a -> r @> a
runCachePure initial = fmap snd . runKVStorePurely initial
