{-# LANGUAGE DataKinds #-}

-- | The Filesystem backend: files on disk as @root\/xxxx\/fileid@. Hits are served
-- straight from the file ('LocalFile'); the file's mtime is the LRU clock, and
-- 'runEvictor' keeps the cache under the disk limit the server assigns.
module Hath.Cache.Filesystem ( FsCache, newFsCache, runCacheFilesystem, runEvictor, sweep ) where

import           Colog                   ( Message )
import           Colog.Polysemy          ( Log )

import           Control.Concurrent      ( threadDelay )

import qualified Data.ByteString.Char8   as BS
import           Data.String.Interpolate ( i )

import           Foreign.C.Types         ( CTime(..) )

import           Hath.AtomicFile         ( writeFileAtomic )
import           Hath.Cache              ( Cache(..), CacheHit(..) )
import           Hath.Error              ( HathError(..) )
import           Hath.FileURI            ( FileURI(..), renderFileURIBytes )
import           Hath.Log                ( logError )

import           Polysemy
import           Polysemy.Error          ( Error, throw )

import           Relude

import           System.Directory        ( createDirectoryIfMissing, listDirectory, removeFile )
import           System.FilePath         ( (</>), takeFileName )
import qualified System.Posix.Files      as Posix

import           UnliftIO.Exception      ( tryAny )

data FsCache
  = FsCache { fsRoot :: !FilePath
            , fsUsed :: !(IORef Int64)
              -- ^ bytes on disk as of the last sweep, plus writes and deletes since
            }

newFsCache :: FilePath -> IO FsCache
newFsCache root = do
  createDirectoryIfMissing True root
  FsCache root <$> newIORef 0

fileURIToPath :: FilePath -> FileURI -> FilePath
fileURIToPath root uri = root </> take 4 fileId </> fileId
  where
    -- no directory traversal through a crafted file id. A file id is ASCII
    -- ('Hath.FileURI.parseFileURI' admits nothing else), so Char8 reads it exactly.
    fileId = takeFileName (BS.unpack (renderFileURIBytes uri))

addUsed :: FsCache -> Int64 -> IO ()
addUsed fs n = atomicModifyIORef' (fsUsed fs) $ \used -> ( used + n, () )

runCacheFilesystem :: Members '[ Embed IO, Log Message, Error HathError ] r
                   => FsCache
                   -> Sem (Cache : r) a
                   -> Sem r a
runCacheFilesystem fs = interpret $ \case
  LookupFile uri      -> embed $ do
    let path = fileURIToPath (fsRoot fs) uri
    tryAny (Posix.getSymbolicLinkStatus path) >>= \case
      Right st
        | Posix.isRegularFile st && fromIntegral (Posix.fileSize st) == fileSize uri -> do
          -- a hit makes the file the newest, so eviction takes it last
          void $ tryAny $ Posix.touchFile path
          pure $ Just $ LocalFile path
        | Posix.isRegularFile st -> do
          -- truncated (written in place by an older version): drop it so it gets refetched
          void $ removeCached fs path st
          pure Nothing
      _        -> pure Nothing

  StoreFile uri bytes -> do
    let path = fileURIToPath (fsRoot fs) uri
    embed (tryAny (writeFileAtomic path bytes)) >>= \case
      Left err -> do
        logError [i|Filesystem write failed for #{path}: #{err}|]
        throw $ StorageError (show err)
      Right () -> embed $ addUsed fs (fromIntegral $ BS.length bytes)

  DeleteFile uri      -> do
    let path = fileURIToPath (fsRoot fs) uri
    embed (tryAny (Posix.getSymbolicLinkStatus path)) >>= \case
      Left _   -> pass
      Right st -> embed (removeCached fs path st) >>= \case
        Left err -> throw $ StorageError [i|Could not delete #{path}: #{err}|]
        Right () -> pass

removeCached :: FsCache -> FilePath -> Posix.FileStatus -> IO (Either SomeException ())
removeCached fs path st = do
  removed <- tryAny (removeFile path)
  whenRight_ removed $ \() -> addUsed fs (negate $ fromIntegral $ Posix.fileSize st)
  pure removed

-- | Evict this far below the limit, so eviction does not run on every write:
-- 100 MiB like the official client, less for tiny limits.
wantFree :: Int64 -> Int64
wantFree limit = min (100 * 1024 * 1024) (limit `div` 10)

-- | Keep the cache under the disk limit (0 = no limit yet), forever. The first
-- sweep learns how much is already on disk; after that we only rescan when over.
runEvictor :: FsCache -> IO Int64 -> IO Void
runEvictor fs getLimit
  = attempt >> infinitely (do
                             threadDelay (10 * 1000000)
                             limit <- getLimit
                             used <- readIORef (fsUsed fs)
                             when (limit > 0 && used > limit) attempt)
  where
    attempt = tryAny (getLimit >>= sweep fs) >>= \case
      Left err           -> putStrLn [i|Cache eviction failed: #{err}|]
      Right ( 0, _ )     -> pass
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
        | limit > 0 && total > limit
          = takeBytes (total - limit + wantFree limit) (sortOn fst3 files)
        | otherwise = []
  freed <- fmap catMaybes $ forM victims $ \( _, size, rel ) -> rightToMaybe . (size <$)
    <$> tryAny (removeFile (fsRoot fs </> decodeUtf8 (fromShort rel)))
  -- keep what was written meanwhile
  atomicModifyIORef' (fsUsed fs) $ \used -> ( used - before + total - sum freed, () )
  pure ( length freed, sum freed )
  where
    fst3 ( t, _, _ ) = t

    takeBytes n (x@( _, size, _ ) : xs)
      | n > 0 = x : takeBytes (n - size) xs
    takeBytes _ _ = []

-- | (mtime, size, path relative to root) of our files: root/xxxx/xxxx….
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
              Just ( mtime, fromIntegral (Posix.fileSize st), toShort (encodeUtf8 rel) )
        _        -> Nothing
