
module Main ( main ) where

import           Cache                   ( Cache(..) )

import           Control.Exception.Safe  ( catch )

import qualified Data.ByteString.Char8   as BSC
import           Data.String.Interpolate ( i )
import qualified Data.Text               as Text

import           Database.SQLite.Simple  ( Only, execute_, query, query_, withConnection )

import           Relude

import           System.IO               ( hPutStrLn )

import           Text.Hex                ( encodeHex )

import           Utils                   ( hathHash )

main :: IO ()
main = do
    [ path ] <- getArgs
    withConnection path $ \conn -> do
        execute_
            conn
            "CREATE TABLE IF NOT EXISTS files (lru_counter INTEGER NOT NULL DEFAULT 0, s4 TEXT NOT NULL, file_id TEXT PRIMARY KEY, file_name TEXT, bytes BLOB NOT NULL) strict"
        fileIds :: [ Only Text ] <- query_ conn "SELECT file_id FROM files"
        for_ fileIds $ \fileId
            -> (do
                    row :: [ Cache ] <- query conn "SELECT * FROM files WHERE file_id = ?" fileId
                    case row of
                        [ r ] -> do
                            let s4' = Text.unpack (cacheS4 r)
                            if take 4 (BSC.unpack (hathHash (cacheBytes r))) == s4'
                                then appendFile
                                    "fix.sql"
                                    [i|INSERT INTO files (lru_counter, s4, file_id, file_name, bytes) VALUES (#{cacheLRUCounter r}, '#{s4'}', '#{cacheFileId r}', '#{fromMaybe "" (cacheFileName r)}', x'#{encodeHex (cacheBytes r)}');
                                    |]
                                else putStrLn $ "File " <> show fileId <> " is invalid"
                        _     -> putStrLn $ "File " <> show fileId <> " is invalid")
            `catch` \(e :: SomeException) -> do
                hPutStrLn stderr $ "Error on file " <> show fileId <> ": " <> show e


