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
  , StorageResult(..)
    -- * Globals
  , hentaiHeader
  , clientBuild
    -- * Default values
  , defaultHathSettings
  , defaultClientConfig
  , emptyFileURI
    -- * Parsing
  , parseSettings
  , parseMetadata
  , readClientConfig
  , parseFileURI
  , renderFileURIBytes
  , renderFileURIText
  , parseRPCResponse
  , parseRPCResponse'
  , verifyRPCResponse
  , reconstructRecord
    -- * Selectors
  , getPayload
  ) where

import qualified Data.ByteString.Char8  as BS
import           Data.Char              ( isAsciiLower, isAsciiUpper, isDigit )
import qualified Data.ByteString.Short  as SBS
import qualified Data.HashSet           as HashSet
import qualified Data.Text              as T

import qualified Mason.Builder          as BD

import           Database.SQLite.Simple ( FromRow, ToRow )

import           Dhall                  ( FromDhall(..), ToDhall(..), auto, input )

import           Network.HTTP.Types     ( HeaderName )

import           Polysemy               ( Member, Sem )
import           Polysemy.Error         ( Error )
import qualified Polysemy.Error         as Error

import           Prelude                ( Show(show) )

import           Relude                 hiding ( show )

import           Utils                  ( hash )

{-# SPECIALISE hentaiHeader :: [ ( HeaderName, Text ) ] #-}
{-# SPECIALISE hentaiHeader :: [ ( HeaderName, ByteString ) ] #-}
hentaiHeader :: IsString a => [ ( HeaderName, a ) ]
hentaiHeader
  = [ ( "Connection", "close" )
    , ( "User-Agent", "Hentai@Home 1.6.6" )
    , ( "Cache-Control", "public, max-age=31536000" )
    , ( "Server", "Genetic Lifeform and Distributed Open Server 1.6.6" )
    , ( "X-Content-Type-Options", "nosniff" )
    ]

-- | The protocol level this client implements (official 1.6.6), sent as clientbuild.
clientBuild :: Text
clientBuild = "181"

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
  , version      :: {-# UNPACK #-} !Text -- ^ unused, kept so existing configs load; see 'clientBuild'
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
  , rpcServerIPs       :: ![ ByteString ]
    -- ^ As sent by the server, e.g. "::ffff:192.0.2.10"
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
  , rpcServerIPs       = []
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
                  ( "rpc_server_ip", _ ) -> s { rpcServerIPs = BS.split ';' v }
                  _ -> s) defaultHathSettings

data GalleryFile
  = GalleryFile { galleryFilePage  :: {-# UNPACK #-} !Int
                , galleryFileIndex :: {-# UNPACK #-} !Int
                , galleryFileName  :: {-# UNPACK #-} !ByteString
                , galleryFileXRes  :: {-# UNPACK #-} !ByteString
                , galleryFileHash  :: !(Maybe ByteString) -- ^ Nothing: generated on demand, unverifiable
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

-- | Parse a /16/dl fetchqueue reply (hathdl format). Left carries the reason it was
-- refused: NO_PENDING_DOWNLOADS and other non-lists, or a list failing the 1.6.6 checks.
-- The file list must carry DLHASH = sha1(client key ++ "\n" ++ line, per file line),
-- and names/types must be ones the gallery system can produce, which keeps a forged
-- list from writing outside the download directory.
parseMetadata :: ByteString -> ByteString -> Either Text GalleryMetadata
parseMetadata clientKey bytes = case break (== "FILELIST") (BS.split '\n' bytes) of
  ( _, [] ) -> Left $ decodeUtf8 $ BS.strip $ BS.take 64 bytes
  ( header, _ : rest ) -> do
    let fileLines = filter (not . BS.null) (takeWhile (/= "INFORMATION") rest)
        field k = listToMaybe [ BS.drop 1 v | l <- header, let ( f, v ) = BS.break (== ' ') l, f == k ]
    gid <- maybeToRight "invalid GID" $ mfilter (> 0) $ readIntExact =<< field "GID"
    minXRes <- maybeToRight "invalid MINXRES" $ mfilter validXRes $ field "MINXRES"
    when (field "DLHASH" /= Just (hash (clientKey <> foldMap ("\n" <>) fileLines)))
      $ Left "DLHASH does not match the file list"
    files <- traverse parseFile fileLines
    pure
      GalleryMetadata
      { galleryID        = gid
      , galleryFileCount = length files
      , galleryMinXRes   = minXRes
      , galleryTitle     = fromMaybe "" (field "TITLE")
      , galleryFileList  = files
      }
  where
    parseFile line = case BS.split ' ' line of
      [ page, index, xres, sha1, ext, name ]
        | Just p <- readIntExact page
        , Just ix <- readIntExact index
        , validName name
        , ext `elem` [ "jpg", "gif", "png", "webp" ] -> Right
          GalleryFile
          { galleryFilePage  = p
          , galleryFileIndex = ix
          , galleryFileName  = name
          , galleryFileXRes  = xres
          , galleryFileHash  = if sha1 == "unknown" then Nothing else Just sha1
          , galleryFileExt   = ext
          }
      _ -> Left $ "invalid file line: " <> decodeUtf8 line

    validName n = not (BS.null n) && BS.length n <= 250 && not (".." `BS.isInfixOf` n)
      && BS.all (\c -> isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("._-" :: String)) n

    validXRes x = x == "org" || (not (BS.null x) && BS.all isDigit x)

    readIntExact s = case BS.readInt s of
      Just ( n, "" ) -> Just n
      _ -> Nothing

data FileURI
  = FileURI { fileHash :: {-# UNPACK #-} !ByteString
            , fileSize :: {-# UNPACK #-} !Int
            , fileXRes :: {-# UNPACK #-} !Int
            , fileYRes :: {-# UNPACK #-} !Int
            , fileExt  :: {-# UNPACK #-} !ByteString
            }
  deriving ( Ord, Eq )

renderFileURIBytes :: FileURI -> ByteString
renderFileURIBytes (FileURI { fileHash, fileSize, fileXRes, fileYRes, fileExt })
  = BD.toStrictByteString
    (BD.byteString fileHash
     <> "-"
     <> BD.intDec fileSize
     <> "-"
     <> BD.intDec fileXRes
     <> "-"
     <> BD.intDec fileYRes
     <> "-"
     <> BD.byteString fileExt)

renderFileURIText :: FileURI -> Text
renderFileURIText = decodeUtf8 . renderFileURIBytes

instance Show FileURI where
  show = toString . renderFileURIText

emptyFileURI :: FileURI
emptyFileURI = FileURI { fileHash = "", fileSize = 0, fileXRes = 0, fileYRes = 0, fileExt = "" }

parseFileURI :: ByteString -> FileURI
parseFileURI bytes = loop 0 0 0 0 0 0
  where
    !len = BS.length bytes

    loop !idx !cnt !i1 !i2 !i3 !i4
      | idx >= len
        = if cnt == (4 :: Int)
          then mkURI i1 i2 i3 i4
          else emptyFileURI
      | BS.index bytes idx == '-' = case cnt of
        0 -> loop (idx + 1) 1 idx 0 0 0
        1 -> loop (idx + 1) 2 i1 idx 0 0
        2 -> loop (idx + 1) 3 i1 i2 idx 0
        3 -> loop (idx + 1) 4 i1 i2 i3 idx
        _ -> emptyFileURI
      | otherwise = loop (idx + 1) cnt i1 i2 i3 i4

    mkURI i1 i2 i3 i4
      = let
          hash = BS.take i1 bytes
          size = BS.take (i2 - i1 - 1) (BS.drop (i1 + 1) bytes)
          xres = BS.take (i3 - i2 - 1) (BS.drop (i2 + 1) bytes)
          yres = BS.take (i4 - i3 - 1) (BS.drop (i3 + 1) bytes)
          ext  = BS.drop (i4 + 1) bytes
        in 
          FileURI { fileHash = hash
                  , fileSize = conv size
                  , fileXRes = conv xres
                  , fileYRes = conv yres
                  , fileExt  = ext
                  }

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

-- | Check the 1.6.6 RPC signature. A signed reply starts with OK-<time>-<sha1>, the sha1
-- covering "<time>-<client key>" then "\n<line>" for every later line. A valid reply
-- comes back with that first line reduced to OK; a forged or unsigned OK becomes
-- RESPONSE_HASH_FAILED; any other status (FAIL, KEY_EXPIRED, ...) passes through.
verifyRPCResponse :: ByteString -> ByteString -> ByteString
verifyRPCResponse clientKey bytes = case BS.split '\n' bytes of
  status : rest
    | Just ( time, sig ) <- BS.break (== '-') <$> BS.stripPrefix "OK-" status
    , sig == "-" <> hash (time <> "-" <> clientKey <> foldMap ("\n" <>) body)
      -> BS.intercalate "\n" ("OK" : body)
    | "OK" `BS.isPrefixOf` status -> "RESPONSE_HASH_FAILED"
    where
      -- Java's split drops trailing empty lines, and the hash is over what it keeps
      body = reverse $ dropWhile BS.null $ reverse rest
  _ -> bytes

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
    fileId = renderFileURIText uri

data StorageResult = Record FileRecord | Redirect ByteString
