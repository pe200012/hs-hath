{-# LANGUAGE DataKinds #-}

module SQLiteSpec ( sqliteSpecs ) where

import qualified Data.ByteString        as BS

import           Database.SQLite.Simple ( Connection, withConnection )

import           Hath.Cache             ( Cache, CacheHit(..), deleteFile, lookupFile, storeFile )
import           Hath.Cache.SQLite      ( initializeDB, runCacheSQLite )
import           Hath.FileURI           ( FileURI(..) )
import           Hath.Protocol          ( sha1Hex )

import           Polysemy               ( Embed, Sem, runM )

import           Relude

import           Test.Hspec

sqliteSpecs :: Spec
sqliteSpecs
  = describe "SQLite cache"
  $ it "deletes a file; deleting one it does not have is no error"
  $ withConnection ":memory:"
  $ \conn -> do
    initializeDB conn
    inCache conn $ storeFile uri content
    inCache conn (lookupFile uri) >>= (`shouldBe` Just (InMemory content))
    inCache conn $ deleteFile uri
    inCache conn (lookupFile uri) >>= (`shouldBe` Nothing)
    inCache conn $ deleteFile uri
  where
    content = "test content for SQLite storage"

    uri
      = FileURI { fileHash = sha1Hex content
                , fileSize = BS.length content
                , fileXRes = 800
                , fileYRes = 600
                , fileExt  = "jpg"
                }

inCache :: Connection -> Sem '[ Cache, Embed IO ] a -> IO a
inCache conn = runM . runCacheSQLite conn
