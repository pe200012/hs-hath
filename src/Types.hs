{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Strict #-}

module Types
    (  -- * Types
      MkClientProxy(..)
    , MkClientConfig(..)
    , GalleryMetadata(..)
    , HathException(..)
    , HathSettings(..)
    , GalleryFile(..)
    , RPCError(..)
    , ClientConfig
    , ClientProxy
    , FileURI(..)
    , RPCResponse(..)
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
      -- * Selectors
    , getPayload
    ) where

import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Short   as SBS
import qualified Data.HashSet            as HashSet
import           Data.String.Interpolate ( i )
import qualified Data.Text.Encoding      as TE
import           Data.Tuple.Extra        ( both )

import           Dhall                   ( FromDhall(..), ToDhall(..), auto, input )

import           Network.HTTP.Types      ( HeaderName )

import           Polysemy                ( Member, Sem )
import           Polysemy.Error          ( Error )
import qualified Polysemy.Error          as Error

import           Prelude                 ( show )

import           Relude

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

data HathException = GracefulShutdown | UnrecoverableError SomeException | HotReload
    deriving ( Show )

instance Exception HathException

data MkClientProxy t
    = MkClientProxy
    { host :: !t, port :: {-# UNPACK #-} !Integer, auth :: {-# UNPACK #-} !(Maybe ( t, t )) }
    deriving ( Show, Generic )

data MkClientConfig t
    = MkClientConfig { clientId    :: !t
                     , key         :: !t
                     , version     :: !t
                     , proxy       :: {-# UNPACK #-} !(Maybe (MkClientProxy t))
                     , downloadDir :: !t
                     , cachePath   :: !t
                     }
    deriving ( Show, Generic )

instance FromDhall t => FromDhall (MkClientProxy t)

instance FromDhall t => FromDhall (MkClientConfig t)

instance ToDhall t => ToDhall (MkClientProxy t)

instance ToDhall t => ToDhall (MkClientConfig t)

type ClientConfig = MkClientConfig ShortByteString

type ClientProxy = MkClientProxy ShortByteString

defaultClientConfig :: ClientConfig
defaultClientConfig
    = MkClientConfig
    { clientId = "", key = "", version = "", proxy = Nothing, downloadDir = "", cachePath = "" }

readClientConfig :: Text -> IO ClientConfig
readClientConfig path = do
    MkClientConfig { clientId, key, version, proxy, downloadDir, cachePath } <- input auto path
    pure
        MkClientConfig
        { clientId = t2bs clientId, key = t2bs key, version = t2bs version, proxy = case proxy of
              Just (MkClientProxy { host, port, auth }) -> Just
                  (MkClientProxy { host = t2bs host, port = port, auth = fmap (both t2bs) auth })
              Nothing -> Nothing, downloadDir = t2bs downloadDir, cachePath = t2bs cachePath }
  where
    t2bs = SBS.toShort . TE.encodeUtf8

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

data RPCResponse = RPCResponse { statusCode :: !ByteString, payload :: ![ ByteString ] }
    deriving ( Show, Eq, Generic )

data RPCError
    = EmptyResponse
    | RequestFailure Text  -- Contains the error status code
    | CertificateFailure Text
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
