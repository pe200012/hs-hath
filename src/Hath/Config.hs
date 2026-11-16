-- | The @client-login@ file: who we are and where the cache lives.
module Hath.Config
  ( ClientConfig(..)
  , ClientProxy(..)
  , CacheBackend(..)
  , R2Config(..)
  , defaultClientConfig
  , readClientConfig
  ) where

import           Dhall  ( FromDhall, ToDhall, auto, input )

import           Relude

-- | The field names are the keys of the Dhall record, and the record must match
-- exactly, so removing or renaming a field breaks every existing config file.
-- That is why the fields nothing reads are still here.
data ClientConfig
  = ClientConfig
  { clientId     :: !Text
  , key          :: !Text
    -- ^ The client key: it signs our requests and proves the server's replies.
  , version      :: !Text
    -- ^ Not read; the build we announce is 'Hath.Protocol.clientBuild'.
  , proxy        :: !(Maybe ClientProxy)
    -- ^ Not read; no request goes through a proxy.
  , downloadDir  :: !Text
    -- ^ Where the gallery downloader puts its files.
  , cachePath    :: !Text
    -- ^ A directory (Filesystem) or a database file (SQLite); R2 has no use for it.
  , cacheBackend :: !CacheBackend
  , r2Config     :: !(Maybe R2Config)
  , lruCacheSize :: !Int64
    -- ^ Not read.
  }
  deriving ( Show, Generic )

instance FromDhall ClientConfig

instance ToDhall ClientConfig

data ClientProxy = ClientProxy { host :: !Text, port :: !Integer, auth :: !(Maybe ( Text, Text )) }
  deriving ( Show, Generic )

instance FromDhall ClientProxy

instance ToDhall ClientProxy

data CacheBackend = CacheBackendSQLite | CacheBackendR2 | CacheBackendFilesystem
  deriving ( Show, Eq, Generic )

instance FromDhall CacheBackend

instance ToDhall CacheBackend

-- | Where the bucket is. Its credentials come from the environment
-- (@R2_ACCESS_KEY@, @R2_SECRET_KEY@), not from the config file.
data R2Config = R2Config { r2Endpoint :: !Text, r2Bucket :: !Text }
  deriving ( Show, Generic )

instance FromDhall R2Config

instance ToDhall R2Config

defaultClientConfig :: ClientConfig
defaultClientConfig
  = ClientConfig
  { clientId     = ""
  , key          = ""
  , version      = ""
  , proxy        = Nothing
  , downloadDir  = ""
  , cachePath    = ""
  , cacheBackend = CacheBackendSQLite
  , r2Config     = Nothing
  , lruCacheSize = 100 * 1024 * 1024
  }

-- | Evaluate a Dhall expression, usually a path such as @./client-login@.
readClientConfig :: Text -> IO ClientConfig
readClientConfig = input auto
