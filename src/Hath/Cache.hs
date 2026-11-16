{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The file cache as the rest of the client sees it. The backends
-- ("Hath.Cache.Filesystem", "Hath.Cache.R2", "Hath.Cache.SQLite") interpret it.
module Hath.Cache ( Cache(..), CacheHit(..), lookupFile, storeFile, CacheRunner(..) ) where

import           Colog          ( Message )
import           Colog.Polysemy ( Log )

import           Hath.Error     ( HathError )
import           Hath.FileURI   ( FileURI )

import           Polysemy
import           Polysemy.Error ( Error )

import           Relude

-- | How a cached file reaches the reader. Each backend has its own way.
data CacheHit
  = InMemory !ByteString
    -- ^ the content itself
  | Redirect !ByteString
    -- ^ a URL to send the reader to
  | LocalFile !FilePath
    -- ^ a file to send straight from disk
  deriving ( Eq, Show )

data Cache m a where
  LookupFile :: FileURI -> Cache m (Maybe CacheHit)
  -- | Keep a file whose content has already been verified against its id.
  StoreFile :: FileURI -> ByteString -> Cache m ()

makeSem ''Cache

-- | A backend chosen at run time.
newtype CacheRunner
  = CacheRunner
  { runCacheWith :: forall r a. Members '[ Embed IO, Log Message, Error HathError ] r
                 => Sem (Cache : r) a
                 -> Sem r a
  }
