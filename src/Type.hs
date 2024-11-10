{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Type
    ( -- * Types
      ClientConfig(..)
    , ClientProxy(..)
    , ConfigText(..)
    , GalleryMetadata(..)
    , HathException(..)
    , HathSettings(..)
    , GalleryFile(..)
    , HathM(..)
    , GlobalContext(..)
      -- * Default values
    , defaultHathSettings
      -- * Parsing
    , parseMetadata
      -- * HathM handler
    , runHath
    ) where

import           Colog                  ( HasLog(..), LogAction, Message, richMessageAction )

import           Control.Monad.Catch    ( MonadThrow )

import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Short  as SBS
import qualified Data.Text.Encoding     as TE

import           Database.SQLite.Simple ( Connection )

import           Dhall                  ( FromDhall(..), ToDhall(..), auto, inject )

import           Network.TLS            ( Credential )

import           Relude

import           UnliftIO               ( MonadUnliftIO )

data HathException = GracefulShutdown | UnrecoverableError SomeException | HotReload
    deriving ( Show )

instance Exception HathException

-- Newtype wrapper for ShortByteString
newtype ConfigText = ConfigText { unConfigText :: ShortByteString }
    deriving ( Show, Generic )

instance FromDhall ConfigText where
    autoWith _ = ConfigText . SBS.toShort . TE.encodeUtf8 <$> Dhall.auto

instance ToDhall ConfigText where
    injectWith _ = (TE.decodeUtf8 . SBS.fromShort . unConfigText) >$< Dhall.inject

data ClientProxy
    = ClientProxy { host :: {-# UNPACK #-} !ConfigText
                  , port :: {-# UNPACK #-} !Integer
                  , auth :: {-# UNPACK #-} !(Maybe ( ConfigText, ConfigText ))
                  }
    deriving ( Show, Generic )

data ClientConfig
    = ClientConfig { clientId    :: {-# UNPACK #-} !ConfigText
                   , key         :: {-# UNPACK #-} !ConfigText
                   , version     :: {-# UNPACK #-} !ConfigText
                   , proxy       :: {-# UNPACK #-} !(Maybe ClientProxy)
                   , downloadDir :: {-# UNPACK #-} !ConfigText
                   , cachePath   :: {-# UNPACK #-} !ConfigText
                   }
    deriving ( Show, Generic )

instance FromDhall ClientProxy

instance FromDhall ClientConfig

instance ToDhall ClientProxy

instance ToDhall ClientConfig

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
