{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Storage.Filesystem ( runCacheFilesystem ) where

import           Colog              ( Message, Severity(Error) )
import           Colog.Polysemy     ( Log )

import           Control.Exception  ( try )

import qualified Data.ByteString    as BS
import           Data.Cache.LRU.IO
import qualified Data.Cache.LRU.IO  as LRU
import qualified Data.Text          as T

import           Polysemy
import           Polysemy.Error     ( Error, throw )
import           Polysemy.KVStore   ( KVStore(..) )
import           Polysemy.Operators
import           Polysemy.Reader    ( Reader, asks )

import           Relude             hiding ( Reader, ask, asks )

import           Storage.Database   ( FileRecord(..) )

import           System.Directory   ( createDirectoryIfMissing, doesFileExist, removeFile )
import           System.FilePath    ( (</>), takeDirectory, takeFileName )

import           Types              ( ClientConfig, FileURI(..), RPCError(..) )
import qualified Types              as Ty

import           Utils              ( log )

-- | Generate filesystem path from root and FileURI
-- Format: root/s4/fileid
fileURIToPath :: FilePath -> FileURI -> FilePath
fileURIToPath root uri = root </> s4 </> fileId
  where
    -- Sanitize to prevent directory traversal
    fileId = takeFileName (show uri)

    s4     = T.unpack $ T.take 4 (T.pack fileId)

-- | Run the cache with Filesystem backend
runCacheFilesystem :: Members '[ Embed IO, Log Message, Reader ClientConfig, Error RPCError ] r
                   => FilePath -- ^ Cache root directory
                   -> KVStore FileURI FileRecord : r @> a
                   -> r @> a
runCacheFilesystem root m = do
  lruSize <- asks Ty.lruCacheSize
  let entries = Just (fromIntegral (lruSize `div` 300000)) -- Approximate number of entries based on avg file size ~300 KiB
  memCache <- embed $ newAtomicLRU entries
  interpret (phi memCache) m
  where
    phi :: forall r r1 x. Members '[ Embed IO, Log Message, Error RPCError ] r1
        => AtomicLRU FileURI FileRecord
        -> KVStore FileURI FileRecord (Sem r) x
        -> Sem r1 x
    phi cache (LookupKV uri) = do
      let path = fileURIToPath root uri
      embed (LRU.lookup uri cache) >>= \case
        Just res -> pure $ Just res
        Nothing  -> do
          result <- embed $ try @SomeException $ BS.readFile path
          case result of
            Left _      -> pure Nothing -- File not found or read error
            Right bytes -> do
              let recd = Ty.reconstructRecord uri bytes
              embed $ LRU.insert uri recd cache
              pure $ Just recd

    phi cache (UpdateKV uri (Just record)) = do
      let path    = fileURIToPath root uri
          content = fileRecordBytes record
      result <- embed $ try @SomeException $ do
        createDirectoryIfMissing True (takeDirectory path)
        BS.writeFile path content
      case result of
        Left err -> do
          log Error ("Filesystem write failed for " <> T.pack path <> ": " <> T.pack (show err))
          throw $ Ty.StorageError (T.pack $ show err)
        Right _  -> embed $ LRU.insert uri record cache

    phi cache (UpdateKV uri Nothing) = do
      let path = fileURIToPath root uri
      result <- embed $ try @SomeException $ do
        exists <- doesFileExist path
        when exists $ removeFile path
      case result of
        Left err -> do
          log Error ("Filesystem delete failed for " <> T.pack path <> ": " <> T.pack (show err))
          throw $ Ty.StorageError (T.pack $ show err)
        Right _  -> embed $ void $ LRU.delete uri cache
