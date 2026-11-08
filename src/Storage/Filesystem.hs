{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- | Files on disk as root/xxxx/fileid. Hits are served straight from the file
-- ('LocalFile'); the file's mtime is the LRU clock, and 'runEvictor' keeps the
-- cache under the disk limit the server assigns.
module Storage.Filesystem ( FsCache, newFsCache, runCacheFilesystem, runEvictor, sweep ) where

import           Colog                 ( Message, Severity(Error) )
import           Colog.Polysemy        ( Log )

import           Control.Concurrent    ( threadDelay )

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Short as SBS
import           Data.String.Interpolate ( i )

import           Foreign.C.Types       ( CTime(..) )

import           Polysemy
import           Polysemy.Error        ( Error, throw )
import           Polysemy.KVStore      ( KVStore(..) )
import           Polysemy.Operators

import           Relude

import           Storage.Database      ( FileRecord(..) )

import           System.Directory      ( createDirectoryIfMissing, listDirectory, removeFile )
import           System.FilePath       ( (</>), takeFileName )
import qualified System.Posix.Files    as Posix

import           Types                 ( FileURI(..), RPCError(..), StorageResult(..), renderFileURIText )

import           UnliftIO.Exception    ( tryAny )

import           Utils                 ( log, writeFileAtomic )

data FsCache = FsCache { fsRoot :: !FilePath, fsUsed :: !(IORef Int64) }
  -- ^ bytes on disk as of the last sweep, plus writes and deletes since

newFsCache :: FilePath -> IO FsCache
newFsCache root = do
  createDirectoryIfMissing True root
  FsCache root <$> newIORef 0

fileURIToPath :: FilePath -> FileURI -> FilePath
fileURIToPath root uri = root </> take 4 fileId </> fileId
  where
    -- no directory traversal through a crafted file id
    fileId = takeFileName (toString $ renderFileURIText uri)

addUsed :: FsCache -> Int64 -> IO ()
addUsed fs n = atomicModifyIORef' (fsUsed fs) $ \used -> ( used + n, () )

runCacheFilesystem :: Members '[ Embed IO, Log Message, Error RPCError ] r
                   => FsCache
                   -> KVStore FileURI StorageResult : r @> a
                   -> r @> a
runCacheFilesystem fs = interpret $ \case
  LookupKV uri -> embed $ do
    let path = fileURIToPath (fsRoot fs) uri
    tryAny (Posix.getSymbolicLinkStatus path) >>= \case
      Right st
        | Posix.isRegularFile st && fromIntegral (Posix.fileSize st) == fileSize uri -> do
          -- a hit makes the file the newest, so eviction takes it last
          void $ tryAny $ Posix.touchFile path
          pure $ Just $ LocalFile path
        | Posix.isRegularFile st -> do
          -- truncated (written in place by an older version): drop it so it gets refetched
          removed <- tryAny (removeFile path)
          whenRight_ removed $ \() -> addUsed fs (negate $ fromIntegral $ Posix.fileSize st)
          pure Nothing
      _ -> pure Nothing

  UpdateKV uri (Just (Record record)) -> do
    let path  = fileURIToPath (fsRoot fs) uri
        bytes = fileRecordBytes record
    embed (tryAny (writeFileAtomic path bytes)) >>= \case
      Left err -> do
        log Error [i|Filesystem write failed for #{path}: #{err}|]
        throw $ StorageError (show err)
      Right () -> embed $ addUsed fs (fromIntegral $ BS.length bytes)

  UpdateKV _ (Just _) -> error "impossible: the Filesystem backend only stores records"

  UpdateKV uri Nothing -> do
    let path = fileURIToPath (fsRoot fs) uri
    embed (tryAny (Posix.getSymbolicLinkStatus path)) >>= \case
      Left _   -> pure () -- not cached
      Right st -> embed (tryAny (removeFile path)) >>= \case
        Left err -> do
          log Error [i|Filesystem delete failed for #{path}: #{err}|]
          throw $ StorageError (show err)
        Right () -> embed $ addUsed fs (negate $ fromIntegral $ Posix.fileSize st)

-- | Evict this far below the limit, so eviction does not run on every write:
-- 100 MiB like the official client, less for tiny limits.
wantFree :: Int64 -> Int64
wantFree limit = min (100 * 1024 * 1024) (limit `div` 10)

-- | Keep the cache under the disk limit (0 = no limit yet). The first sweep
-- learns how much is already on disk; after that we only rescan when over.
runEvictor :: FsCache -> IO Int64 -> IO ()
runEvictor fs getLimit = attempt >> forever (do
  threadDelay (10 * 1000000)
  limit <- getLimit
  used <- readIORef (fsUsed fs)
  when (limit > 0 && used > limit) attempt)
  where
    attempt = tryAny (getLimit >>= sweep fs) >>= \case
      Left err -> putStrLn [i|Cache eviction failed: #{err}|]
      Right ( 0, _ ) -> pure ()
      Right ( n, bytes ) -> putStrLn
        [i|Evicted #{n} least recently served files (#{bytes `div` 1048576} MiB) to stay under the disk limit|]

-- | Rescan the cache; if it is over the limit, delete the least recently served
-- files until it is 'wantFree' under. Returns the files and bytes evicted.
-- ponytail: a full scan per sweep, fine for ~10^5 files; keep an index if caches grow far past that
sweep :: FsCache -> Int64 -> IO ( Int, Int64 )
sweep fs limit = do
  before <- readIORef (fsUsed fs)
  files <- scanCache (fsRoot fs)
  let total   = sum [ size | ( _, size, _ ) <- files ]
      victims
        | limit > 0 && total > limit = takeBytes (total - limit + wantFree limit) (sortOn fst3 files)
        | otherwise = []
  freed <- fmap catMaybes $ forM victims $ \( _, size, rel ) ->
    rightToMaybe . (size <$) <$> tryAny (removeFile (fsRoot fs </> decodeUtf8 (SBS.fromShort rel)))
  -- keep what was written meanwhile (a file written mid-scan may count twice;
  -- that only costs an extra rescan)
  atomicModifyIORef' (fsUsed fs) $ \used -> ( used - before + total - sum freed, () )
  pure ( length freed, sum freed )
  where
    fst3 ( t, _, _ ) = t

    takeBytes n (x@( _, size, _ ) : xs)
      | n > 0 = x : takeBytes (n - size) xs
    takeBytes _ _ = []

-- | (mtime, size, path relative to root) of our files: root/xxxx/xxxx…. Anything
-- else under root is left alone.
scanCache :: FilePath -> IO [ ( Int64, Int64, ShortByteString ) ]
scanCache root = do
  dirs <- filter ((== 4) . length) <$> listDirectory root
  fmap concat $ forM dirs $ \dir -> do
    names <- fromRight [] <$> tryAny (listDirectory (root </> dir))
    fmap catMaybes $ forM (filter (dir `isPrefixOf`) names) $ \name -> do
      let rel = dir </> name
      tryAny (Posix.getSymbolicLinkStatus (root </> rel)) <&> \case
        Right st
          | Posix.isRegularFile st -> let
              CTime mtime = Posix.modificationTime st
            in
              Just ( mtime, fromIntegral (Posix.fileSize st), SBS.toShort (encodeUtf8 rel) )
        _ -> Nothing
