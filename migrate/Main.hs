{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main ( main ) where

import           Cache                   ( Cache(..) )

import           Control.Concurrent      ( getNumCapabilities )
import           Control.Exception       ( SomeException, catch )
import           Control.Monad           ( forM )

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BSC
import           Data.Foldable           ( for_ )
import qualified Data.HashSet            as HashSet
import           Data.List               ( (\\) )
import           Data.String.Interpolate ( i )
import qualified Data.Text               as Text
import           Data.Text.Encoding      ( decodeUtf8 )

import           Database.SQLite.Simple  ( Connection(Connection)
                                         , execute
                                         , execute_
                                         , query
                                         , withConnection
                                         )

import           System.Directory        ( getDirectoryContents )
import           System.Environment      ( getArgs )
import           System.FilePath         ( (</>), takeBaseName )
import           System.Posix            ( getFileStatus, isDirectory )

import           Text.Printf             ( printf )

import           UnliftIO                ( mapConcurrently_, pooledMapConcurrentlyN_ )

import           Utils                   ( hathHash )

-- | Traverse from 'top' directory and return all the files by
-- filtering out the 'exclude' predicate.
traverseDir :: FilePath -> (FilePath -> Bool) -> IO [ FilePath ]
traverseDir top exclude = do
    ds <- getDirectoryContents top
    paths <- forM (filter (not . exclude) ds) $ \d -> do
        if d == "." || d == ".."
            then return []
            else do
                let path = top </> d
                s <- getFileStatus path
                if isDirectory s
                    then traverseDir path exclude
                    else return [ path ]
    return (concat paths)

iterateDir :: FilePath -> (FilePath -> Bool) -> (FilePath -> IO ()) -> IO ()
iterateDir top exclude action = do
    ds <- getDirectoryContents top
    for_ (filter (not . exclude) ds) $ \d -> do
        if d == "." || d == ".."
            then return ()
            else do
                let path = top </> d
                s <- getFileStatus path
                if isDirectory s
                    then iterateDir path exclude action
                    else action path

main :: IO ()
main = do
    [ path ] <- getArgs
    files <- traverseDir path (const False)
    cpucores <- getNumCapabilities
    let filenum   = length files
        groupSize = filenum `div` cpucores
    withConnection "cache.db" $ \conn -> do
        execute_
            conn
            "CREATE TABLE IF NOT EXISTS files (lru_counter INTEGER NOT NULL DEFAULT 0, s4 TEXT NOT NULL, file_id TEXT PRIMARY KEY, file_name TEXT, bytes BLOB NOT NULL)"
        already <- HashSet.fromList . fmap (Text.unpack . cacheFileId)
            <$> query conn "SELECT * FROM files" ()
        pooledMapConcurrentlyN_
            cpucores
            (process conn)
            (filter (\f -> not (takeBaseName f `HashSet.member` already)) files)
  where
    sliceGroups :: Int -> [ a ] -> [ [ a ] ]
    sliceGroups _ [] = []
    sliceGroups n xs
        = let
            ( ys, zs ) = splitAt n xs
            in 
                ys : sliceGroups n zs

    process :: Connection -> FilePath -> IO ()
    process conn f = do
        print f
        bytes <- BS.readFile f
        let hash     = BSC.pack $ takeWhile (/= '-') $ takeBaseName f
            computed = hathHash bytes
            cache
                = Cache { cacheLRUCounter = 0
                        , cacheS4         = decodeUtf8 $ BS.take 4 computed
                        , cacheFileId     = Text.pack $ takeBaseName f
                        , cacheFileName   = Nothing
                        , cacheBytes      = bytes
                        }
        if hash /= computed
            then printf
                [i|File #{f} has a hash mismatch: expected #{hash}, got #{computed}. Skipping...#{'\n'}|]
            else execute
                conn
                "INSERT INTO files (lru_counter, s4, file_id, file_name, bytes) VALUES (?, ?, ?, ?, ?)"
                cache
                `catch` \(e :: SomeException)
                -> printf [i|Error inserting #{f}: #{e}. Skipping...#{'\n'}|]

    worker :: Connection -> [ FilePath ] -> IO ()
    worker = mapM_ . process

