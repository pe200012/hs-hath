{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module FilesystemSpec ( filesystemSpecs ) where

import           Colog                 ( LogAction(..), Message )
import           Colog.Polysemy        ( Log, runLogAction )

import qualified Data.ByteString       as BS
import           Data.Time.Clock.POSIX ( posixSecondsToUTCTime )

import           Polysemy
import           Polysemy.Error        ( Error, runError )
import           Polysemy.KVStore      ( KVStore, lookupKV, updateKV )

import           Relude

import           Storage.Filesystem    ( FsCache, newFsCache, runCacheFilesystem, sweep )

import           System.Directory      ( createDirectoryIfMissing, doesFileExist, setModificationTime )
import           System.FilePath       ( (</>) )
import           System.IO.Temp        ( withSystemTempDirectory )

import           Test.Hspec

import           Types                 ( FileURI, RPCError, StorageResult(..), parseFileURI, reconstructRecord )

import           Utils                 ( hash )

-- | A URI whose hash and size match the content, as the server hands them out.
uriFor :: ByteString -> FileURI
uriFor content = parseFileURI $ hash content <> "-" <> show (BS.length content) <> "-800-600-jpg"

runFs :: FsCache
      -> Sem '[ KVStore FileURI StorageResult, Log Message, Error RPCError, Embed IO ] a
      -> IO a
runFs fs program = do
  result <- runM
    . runError @RPCError
    . runLogAction @IO @Message (LogAction $ \_ -> pure ())
    . runCacheFilesystem fs
    $ program
  either (\e -> fail $ "unexpected error: " <> show e) pure result

store :: FsCache -> ByteString -> IO ()
store fs content = runFs fs $ updateKV (uriFor content) (Just $ Record $ reconstructRecord (uriFor content) content)

-- | The path a hit is served from.
served :: FsCache -> ByteString -> IO (Maybe FilePath)
served fs content = runFs fs (lookupKV (uriFor content)) <&> \case
  Just (LocalFile path) -> Just path
  _ -> Nothing

withCache :: (FilePath -> FsCache -> IO a) -> IO a
withCache k = withSystemTempDirectory "hath-test" $ \tmp -> do
  let root = tmp </> "cache"
  newFsCache root >>= k root

filesystemSpecs :: Spec
filesystemSpecs = describe "Filesystem cache" $ do
  let content = "test content for Filesystem storage"

  it "stores a file and serves hits straight from disk" $ withCache $ \_ fs -> do
    store fs content
    served fs content >>= \case
      Nothing   -> expectationFailure "expected a hit"
      Just path -> readFileBS path >>= (`shouldBe` content)

  it "misses a file it never stored" $ withCache $ \_ fs -> served fs content >>= (`shouldBe` Nothing)

  it "deletes a file" $ withCache $ \_ fs -> do
    store fs content
    Just path <- served fs content
    runFs fs $ updateKV (uriFor content) (Nothing :: Maybe StorageResult)
    doesFileExist path >>= (`shouldBe` False)
    served fs content >>= (`shouldBe` Nothing)

  it "drops a truncated file instead of serving it" $ withCache $ \_ fs -> do
    store fs content
    Just path <- served fs content
    writeFileBS path (BS.take 5 content)
    served fs content >>= (`shouldBe` Nothing)
    doesFileExist path >>= (`shouldBe` False)

  it "evicts the least recently served files, and only its own" $ withCache $ \root fs -> do
    let ( a, b, c ) = ( BS.replicate 1000 97, BS.replicate 1000 98, BS.replicate 1000 99 )
    paths <- forM [ a, b, c ] $ \x -> store fs x >> fromMaybe (error "stored") <$> served fs x
    forM_ (zip paths [ 100, 200, 300 ]) $ \( p, t ) -> setModificationTime p (posixSecondsToUTCTime t)
    -- serving a makes it the newest, so b is now the least recently served
    void $ served fs a
    createDirectoryIfMissing True (root </> "abcd")
    writeFileBS (root </> "notes.txt") "not ours"
    writeFileBS (root </> "abcd" </> "other") "not ours"
    -- 3000 bytes against a 2500 byte limit (evicting down to 2250): one file has to go
    sweep fs 2500 >>= (`shouldBe` ( 1, 1000 ))
    mapM doesFileExist paths >>= (`shouldBe` [ True, False, True ])
    mapM doesFileExist [ root </> "notes.txt", root </> "abcd" </> "other" ] >>= (`shouldBe` [ True, True ])

  it "evicts nothing under the limit, or without one" $ withCache $ \_ fs -> do
    store fs content
    sweep fs 0 >>= (`shouldBe` ( 0, 0 ))
    sweep fs 1000000 >>= (`shouldBe` ( 0, 0 ))
