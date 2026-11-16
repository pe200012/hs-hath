module Hath.AtomicFile ( writeFileAtomic ) where

import           Control.Exception ( bracketOnError, try )

import qualified Data.ByteString   as BS

import           Relude

import           System.Directory  ( createDirectoryIfMissing, removeFile, renameFile )
import           System.FilePath   ( takeDirectory, takeFileName )
import           System.IO         ( hClose, openBinaryTempFile )

-- | Write through a temp file in the same directory, then rename: a crash or a
-- concurrent reader never sees a partial file. Missing directories are created.
writeFileAtomic :: FilePath -> ByteString -> IO ()
writeFileAtomic path bytes = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  bracketOnError
    (openBinaryTempFile dir (takeFileName path <> ".tmp"))
    (\( tmp, h ) -> hClose h >> void (try @SomeException (removeFile tmp)))
    $ \( tmp, h ) -> BS.hPut h bytes >> hClose h >> renameFile tmp path
