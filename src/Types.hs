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
    , HathM(..)
    , GlobalContext(..)
    , ClientConfig
    , ClientProxy
    , FileURI(..)
      -- * Default values
    , defaultHathSettings
      -- * Parsing
    , parseMetadata
    , readClientConfig
      -- * HathM handler
    , runHath
    ) where

import           Colog                   ( HasLog(..), LogAction, Message, richMessageAction )

import           Control.Monad.Catch     ( MonadThrow )

import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.ByteString.Short   as SBS
import           Data.String.Interpolate ( i )
import qualified Data.Text.Encoding      as TE
import           Data.Tuple.Extra        ( both )

import           Database.SQLite.Simple  ( Connection )

import           Dhall                   ( FromDhall(..), ToDhall(..), auto, input )

import           Network.TLS             ( Credential )

import           Prelude                 ( show )

import           Relude

import           UnliftIO                ( MonadUnliftIO )

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

data GlobalContext m msg
    = GlobalContext
    { clientConfig    :: {-# UNPACK #-} !ClientConfig
    , hathSettings    :: {-# UNPACK #-} !(IORef HathSettings)
    , shutdownRequest :: {-# UNPACK #-} !(MVar HathException)
    , logAction       :: !(LogAction m msg)
    , database        :: {-# UNPACK #-} !Connection
    , credential      :: {-# UNPACK #-} !(IORef Credential)
    , galleryTask     :: {-# UNPACK #-} !(MVar ())
    }

instance HasLog (GlobalContext m msg) msg m where
    getLogAction = logAction

    {-# INLINE getLogAction #-}

    setLogAction l s = s { logAction = l }

    {-# INLINE setLogAction #-}

instance Monad m => MonadReader (GlobalContext (HathM m) Message) (HathM m) where
    ask = HathM ask

    {-# INLINE ask #-}

    local f (HathM m) = HathM (local f m)

    {-# INLINE local #-}

newtype HathM m a = HathM { runHathM :: ReaderT (GlobalContext (HathM m) Message) m a }
    deriving newtype ( Functor, Applicative, Monad, MonadThrow, MonadIO, MonadUnliftIO )

runHath :: MonadIO m
        => ClientConfig
        -> IORef HathSettings
        -> Connection
        -> MVar HathException
        -> IORef Credential
        -> MVar ()
        -> HathM m a
        -> m a
runHath cfg hRef conn refreshCert credRef queue m = do
    runReaderT
        (runHathM m)
        (GlobalContext
         { clientConfig    = cfg
         , hathSettings    = hRef
         , shutdownRequest = refreshCert
         , logAction       = richMessageAction
         , database        = conn
         , credential      = credRef
         , galleryTask     = queue
         })

{-# INLINE runHath #-}

data GalleryFile
    = GalleryFile { galleryFilePage  :: {-# UNPACK #-} !Int
                  , galleryFileIndex :: {-# UNPACK #-} !Int
                  , galleryFileName  :: {-# UNPACK #-} !ByteString
                  , galleryFileXRes  :: {-# UNPACK #-} !ByteString
                  , galleryFileHash  :: {-# UNPACK #-} !ByteString
                  , galleryFileExt   :: {-# UNPACK #-} !ByteString
                  }

data GalleryMetadata
    = GalleryMetadata
    { galleryID        :: {-# UNPACK #-} !Int
    , galleryFileCount :: {-# UNPACK #-} !Int
    , galleryMinXRes   :: {-# UNPACK #-} !ByteString
    , galleryTitle     :: {-# UNPACK #-} !ByteString
    , galleryFileList  :: {-# UNPACK #-} ![ GalleryFile ]
    }

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

parseMetadata :: LBS.ByteString -> GalleryMetadata
parseMetadata (LBS.toStrict -> bytes) = foldl' go emptyMetadata (BS.lines bytes)
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

instance Show FileURI where
    show (FileURI { fileHash, fileSize, fileXRes, fileYRes, fileExt })
        = [i|#{fileHash}-#{fileSize}-#{fileXRes}-#{fileYRes}-#{fileExt}|]
