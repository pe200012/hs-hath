{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Strict #-}

module Types
  (  -- * Types
    ClientProxy(..)
  , ClientConfig(..)
  , GalleryMetadata(..)
  , HathSettings(..)
  , GalleryFile(..)
  , RPCError(..)
  , FileURI(..)
  , RPCResponse(..)
  , CacheBackend(..)
  , R2Config(..)
  , FileRecord(..)
    -- * Globals
  , hentaiHeader
    -- * Default values
  , defaultHathSettings
  , defaultClientConfig
  , emptyFileURI
  , emptyMetadata
    -- * Parsing
  , parseSettings
  , parseMetadata
  , readClientConfig
  , parseFileURI
  , parseRPCResponse
  , parseRPCResponse'
  , reconstructRecord
    -- * Selectors
  , getPayload
  ) where

import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Short   as SBS
import qualified Data.HashSet            as HashSet
import           Data.String.Interpolate ( i )
import qualified Data.Text               as T

import           Database.SQLite.Simple  ( FromRow, ToRow )

import           Dhall                   ( FromDhall(..), ToDhall(..), auto, input )

import           Network.HTTP.Types      ( HeaderName )

import           Polysemy                ( Member, Sem )
import           Polysemy.Error          ( Error )
import qualified Polysemy.Error          as Error

import           Prelude                 ( Show(show) )

import           Relude                  hiding ( show )

{-# SPECIALISE hentaiHeader :: [ ( HeaderName, Text ) ] #-}
{-# SPECIALISE hentaiHeader :: [ ( HeaderName, ByteString ) ] #-}
hentaiHeader :: IsString a => [ ( HeaderName, a ) ]
hentaiHeader
  = [ ( "Connection", "close" )
    , ( "User-Agent", "Hentai@Home 176" )
    , ( "Cache-Control", "public, max-age=31536000" )
    , ( "Server", "Genetic Lifeform and Distributed Open Server 1.6.4" )
    , ( "X-Content-Type-Options", "nosniff" )
    ]

data ClientProxy
  = ClientProxy { host :: {-# UNPACK #-} !Text
                , port :: {-# UNPACK #-} !Integer
                , auth :: {-# UNPACK #-} !(Maybe ( Text, Text ))
                }
  deriving ( Show, Generic )

instance FromDhall ClientProxy

instance ToDhall ClientProxy

-- | Cache backend selection
data CacheBackend = CacheBackendSQLite | CacheBackendR2 | CacheBackendFilesystem
  deriving ( Show, Eq, Generic )

instance FromDhall CacheBackend

instance ToDhall CacheBackend

-- | R2 configuration (endpoint and bucket from config, secrets from env)
data R2Config = R2Config { r2Endpoint :: {-# UNPACK #-} !Text, r2Bucket :: {-# UNPACK #-} !Text }
  deriving ( Show, Generic )

instance FromDhall R2Config

instance ToDhall R2Config

data ClientConfig
  = ClientConfig
  { clientId     :: {-# UNPACK #-} !Text
  , key          :: {-# UNPACK #-} !Text
  , version      :: {-# UNPACK #-} !Text
  , proxy        :: {-# UNPACK #-} !(Maybe ClientProxy)
  , downloadDir  :: {-# UNPACK #-} !Text
  , cachePath    :: {-# UNPACK #-} !Text
  , cacheBackend :: !CacheBackend
  , r2Config     :: !(Maybe R2Config)
  , lruCacheSize :: {-# UNPACK #-} !Int64
  }
  deriving ( Show, Generic )

instance FromDhall ClientConfig

instance ToDhall ClientConfig

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
  , lruCacheSize = 100 * 1024 * 1024 -- 100 MiB, roughly 100 MiB / 300 KiB per file = 341 entries
  }

readClientConfig :: Text -> IO ClientConfig
readClientConfig = input auto

data HathSettings
  = HathSettings
  { rpcBaseURL         :: {-# UNPACK #-} !ShortByteString
  , clientHost         :: {-# UNPACK #-} !ShortByteString
  , clientPort         :: {-# UNPACK #-} !Int
  , throttleBytes      :: {-# UNPACK #-} !Int64
  , diskLimitBytes     :: {-# UNPACK #-} !Int64
  , diskRemainingBytes :: {-# UNPACK #-} !Int64
  , cacheNeedsRescan   :: {-# UNPACK #-} !Bool
  , cacheNeedsVerify   :: {-# UNPACK #-} !Bool
  , useLessMemory      :: {-# UNPACK #-} !Bool
  , checkIPOrigin      :: {-# UNPACK #-} !Bool
  , floodControl       :: {-# UNPACK #-} !Bool
  , staticRanges       :: {-# UNPACK #-} !(HashSet ShortByteString)
  }
  deriving ( Show )

defaultHathSettings :: HathSettings
defaultHathSettings
  = HathSettings
  { rpcBaseURL         = "/15/rpc"
  , clientHost         = ""
  , clientPort         = 0
  , throttleBytes      = 0
  , diskLimitBytes     = 0
  , diskRemainingBytes = 0
  , cacheNeedsRescan   = False
  , cacheNeedsVerify   = False
  , useLessMemory      = False
  , checkIPOrigin      = True
  , floodControl       = True
  , staticRanges       = mempty
  }

parseSettings :: [ ByteString ] -> HathSettings
parseSettings
  = foldl' (\s kv -> let
                ( k, rest ) = BS.span (/= '=') kv
                v           = BS.drop 1 rest
              in 
                case ( k, readMaybe @Int64 (BS.unpack v) ) of
                  ( "host", _ ) -> s { clientHost = SBS.toShort v }
                  ( "port", Just p ) -> s { clientPort = fromIntegral p }
                  ( "throttle_bytes", Just bytes ) -> s { throttleBytes = bytes }
                  ( "disklimit_bytes", Just bytes ) -> s { diskLimitBytes = bytes }
                  ( "diskremaining_bytes", Just bytes ) -> s { diskRemainingBytes = bytes }
                  ( "static_ranges", _ ) -> s
                    { staticRanges = HashSet.fromList (SBS.toShort <$> BS.split ';' v) }
                  _ -> s) defaultHathSettings

data GalleryFile
  = GalleryFile { galleryFilePage  :: {-# UNPACK #-} !Int
                , galleryFileIndex :: {-# UNPACK #-} !Int
                , galleryFileName  :: {-# UNPACK #-} !ByteString
                , galleryFileXRes  :: {-# UNPACK #-} !ByteString
                , galleryFileHash  :: {-# UNPACK #-} !ByteString
                , galleryFileExt   :: {-# UNPACK #-} !ByteString
                }
  deriving ( Eq )

data GalleryMetadata
  = GalleryMetadata { galleryID        :: {-# UNPACK #-} !Int
                    , galleryFileCount :: {-# UNPACK #-} !Int
                    , galleryMinXRes   :: {-# UNPACK #-} !ByteString
                    , galleryTitle     :: {-# UNPACK #-} !ByteString
                    , galleryFileList  :: {-# UNPACK #-} ![ GalleryFile ]
                    }
  deriving ( Eq )

emptyMetadata :: GalleryMetadata
emptyMetadata
  = GalleryMetadata
  { galleryID        = 0
  , galleryFileCount = 0
  , galleryMinXRes   = ""
  , galleryTitle     = ""
  , galleryFileList  = []
  }

{-# NOINLINE emptyMetadata #-}

parseMetadata :: ByteString -> GalleryMetadata
parseMetadata bytes = foldl' go emptyMetadata (BS.lines bytes)
  where
    go metadata line = case BS.words line of
      [] -> metadata
      [ "GID", maybeGid ]
        -> maybe metadata (\( gid, _ ) -> metadata { galleryID = gid }) (BS.readInt maybeGid)
      [ "FILECOUNT", maybeCount ] -> maybe
        metadata
        (\( count, _ ) -> metadata { galleryFileCount = count })
        (BS.readInt maybeCount)
      [ "MINXRES", xres ] -> metadata { galleryMinXRes = xres }
      ("TITLE" : rest) -> metadata { galleryTitle = BS.unwords rest }
      [ maybePage, maybeFid, mxres, hash, ext, basename ]
        -> case ( BS.readInt maybePage, BS.readInt maybeFid ) of
          ( Just ( page, _ ), Just ( fid, _ ) ) -> metadata
            { galleryFileList = GalleryFile
                { galleryFilePage  = page
                , galleryFileIndex = fid
                , galleryFileName  = basename
                , galleryFileXRes  = mxres
                , galleryFileHash  = hash
                , galleryFileExt   = ext
                }
                : galleryFileList metadata
            }
          _ -> metadata
      _ -> metadata

    {-# INLINE go #-}

data FileURI
  = FileURI { fileHash :: {-# UNPACK #-} !ByteString
            , fileSize :: {-# UNPACK #-} !Int
            , fileXRes :: {-# UNPACK #-} !Int
            , fileYRes :: {-# UNPACK #-} !Int
            , fileExt  :: {-# UNPACK #-} !ByteString
            }
  deriving ( Ord, Eq )

instance Show FileURI where
  show (FileURI { fileHash, fileSize, fileXRes, fileYRes, fileExt })
    = [i|#{fileHash}-#{fileSize}-#{fileXRes}-#{fileYRes}-#{fileExt}|]

emptyFileURI :: FileURI
emptyFileURI = FileURI { fileHash = "", fileSize = 0, fileXRes = 0, fileYRes = 0, fileExt = "" }

parseFileURI :: ByteString -> FileURI
parseFileURI bytes = case BS.split '-' bytes of
  [ hash, size, xres, yres, ext ] -> FileURI
    { fileHash = hash
    , fileSize = conv size
    , fileXRes = conv xres
    , fileYRes = conv yres
    , fileExt  = ext
    }
  _ -> emptyFileURI
  where
    conv = maybe 0 fst . BS.readInt

data RPCResponse
  = RPCResponse { statusCode :: {-# UNPACK #-} !ByteString, payload :: ![ ByteString ] }
  deriving ( Show, Eq, Generic )

data RPCError
  = EmptyResponse
  | RequestFailure {-# UNPACK #-} !Text  -- Contains the error status code
  | CertificateFailure {-# UNPACK #-} !Text
  | StorageError {-# UNPACK #-} !Text    -- Storage write/delete failure
  deriving ( Show, Eq, Generic )

instance Exception RPCError

{-# INLINE parseRPCResponse #-}
-- | Parse an RPC response from a lazy ByteString
-- The first line is the status code, followed by the payload lines
parseRPCResponse :: ByteString -> Either RPCError RPCResponse
parseRPCResponse bytes = case BS.lines bytes of
  [] -> Left EmptyResponse
  status : rest -> Right $ RPCResponse { statusCode = status, payload = rest }

{-# INLINE getPayload #-}
-- | Get the payload if the response was successful
getPayload :: RPCResponse -> Either RPCError [ ByteString ]
getPayload response
  | statusCode response == "OK" = Right $ payload response
  | otherwise = Left $ RequestFailure $ decodeUtf8 $ statusCode response

{-# INLINE parseRPCResponse' #-}
-- | Parse RPC responses effectfully
parseRPCResponse' :: Member (Error RPCError) r => ByteString -> Sem r [ ByteString ]
parseRPCResponse' bytes = Error.fromEither (getPayload =<< parseRPCResponse bytes)

data FileRecord
  = FileRecord { fileRecordLRUCounter :: {-# UNPACK #-} !Int64
               , fileRecordS4         :: {-# UNPACK #-} !Text
               , fileRecordFileId     :: {-# UNPACK #-} !Text
               , fileRecordFileName   :: {-# UNPACK #-} !(Maybe Text)
               , fileRecordBytes      :: !BS.ByteString
               }
  deriving ( Show, Generic, Eq )

instance FromRow FileRecord

instance ToRow FileRecord

-- | Reconstruct FileRecord from FileURI and bytes
reconstructRecord :: FileURI -> ByteString -> FileRecord
reconstructRecord uri bytes
  = FileRecord { fileRecordLRUCounter = 0
               , fileRecordS4         = T.take 4 fileId
               , fileRecordFileId     = fileId
               , fileRecordFileName   = Nothing
               , fileRecordBytes      = bytes
               }
  where
    fileId = T.pack $ show uri
