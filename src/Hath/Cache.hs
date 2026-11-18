{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The file cache as the rest of the client sees it. The backends
-- ("Hath.Cache.Filesystem", "Hath.Cache.R2", "Hath.Cache.SQLite") interpret it.
module Hath.Cache
  ( Cache(..)
  , CacheHit(..)
  , lookupFile
  , storeFile
  , deleteFile
  , CacheRunner(..)
  ) where

import           Colog          ( Message )
import           Colog.Polysemy ( Log )

import           Hath.Error     ( HathError )
import           Hath.FileURI   ( FileURI )

import           Polysemy
import           Polysemy.Error ( Error )

import           Relude

data CacheHit = InMemory !ByteString | Redirect !ByteString | LocalFile !FilePath
  deriving ( Eq, Show )

data Cache m a where
  LookupFile :: FileURI -> Cache m (Maybe CacheHit)
  -- | Keep a file whose content has already been verified against its id.
  StoreFile :: FileURI -> ByteString -> Cache m ()
  -- | Delete a file the network has blacklisted. A file we do not have is no error.
  DeleteFile :: FileURI -> Cache m ()

makeSem ''Cache

newtype CacheRunner
  = CacheRunner { runCacheWith :: forall r a. Members '[ Embed IO, Log Message, Error HathError ] r
                               => Sem (Cache : r) a
                               -> Sem r a
                }
