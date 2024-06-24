
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
    ( HathError(..)
    , ServerAction(..)
    , GracefulShutdown(GracefulShutdown)
    , HathSettings(..)
    , defaultHathSettings
    , ClientProxy(..)
    , ClientConfig(..)
    , defaultClientConfig
    , Singleton(..)
    , HathM(..)
    , runHath
    , GalleryFile(..)
    , GalleryMetadata(..)
    , emptyMetadata
    , parseMetadata
    ) where

import           Colog                      ( HasLog, LogAction, Message, richMessageAction )
import           Colog.Core.Class           ( HasLog(..) )

import           Control.Monad.Catch        ( MonadThrow )

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Database.SQLite.Simple     ( Connection )

import           Dhall                      ( (>*<)
                                            , Decoder
                                            , FromDhall
                                            , ToDhall(injectWith)
                                            , encodeField
                                            , encodeFieldWith
                                            , inject
                                            , record
                                            , recordEncoder
                                            , strictText
                                            )
import qualified Dhall

import           Network.TLS                ( Credential )

import           Relude

import           UnliftIO                   ( Chan, MonadUnliftIO )

data HathError = InitialContactFailure | InvalidClientKey | InvalidCertificate
    deriving ( Show )

data ServerAction = RefreshCert {-# UNPACK #-} !Credential | Reload

data GracefulShutdown = GracefulShutdown
    deriving ( Show )

instance Exception GracefulShutdown

data HathSettings
    = HathSettings
    { rpcBaseURL         :: {-# UNPACK #-} !ByteString
    , clientHost         :: {-# UNPACK #-} !ByteString
    , clientPort         :: {-# UNPACK #-} !Int
    , throttleBytes      :: {-# UNPACK #-} !Int64
    , diskLimitBytes     :: {-# UNPACK #-} !Int64
    , diskRemainingBytes :: {-# UNPACK #-} !Int64
    , cacheNeedsRescan   :: {-# UNPACK #-} !Bool
    , cacheNeedsVerify   :: {-# UNPACK #-} !Bool
    , useLessMemory      :: {-# UNPACK #-} !Bool
    , checkIPOrigin      :: {-# UNPACK #-} !Bool
    , floodControl       :: {-# UNPACK #-} !Bool
    , staticRanges       :: {-# UNPACK #-} !(HashSet ByteString)
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

{-# NOINLINE defaultHathSettings #-}

readableByteString :: Decoder ByteString
readableByteString = encodeUtf8 <$> strictText

data ClientProxy
    = ClientProxy { proxyHost :: {-# UNPACK #-} !ByteString
                  , proxyPort :: {-# UNPACK #-} !Int
                  , proxyAuth :: {-# UNPACK #-} !(Maybe ( ByteString, ByteString ))
                  }

instance FromDhall ClientProxy where
    autoWith _
        = record
            (ClientProxy <$> Dhall.field "host" readableByteString
             <*> Dhall.field "port" Dhall.auto
             <*> Dhall.field
                 "auth"
                 (Dhall.maybe (Dhall.pair readableByteString readableByteString)))

instance ToDhall ClientProxy where
    injectWith _
        = recordEncoder
            (adapt >$< encodeField "host"
             >*< encodeField "port"
             >*< encodeFieldWith
                 "auth"
                 (contramap (fmap (bimap decodeUtf8 decodeUtf8)) (inject @(Maybe ( Text, Text )))))
      where
        adapt (ClientProxy h p a) = ( h, ( p, a ) )

data ClientConfig
    = ClientConfig { clientID      :: {-# UNPACK #-} !ByteString
                   , clientKey     :: {-# UNPACK #-} !ByteString
                   , clientVersion :: {-# UNPACK #-} !ByteString
                   , clientProxy   :: {-# UNPACK #-} !(Maybe ClientProxy)
                   }

instance FromDhall ClientConfig where
    autoWith _
        = record
            (ClientConfig <$> Dhall.field "id" readableByteString
             <*> Dhall.field "key" readableByteString
             <*> Dhall.field "version" readableByteString
             <*> Dhall.field "proxy" (Dhall.maybe Dhall.auto))

instance ToDhall ClientConfig where
    injectWith _
        = recordEncoder
            (adapt >$< encodeField "id"
             >*< encodeField "key"
             >*< encodeField "version"
             >*< encodeField "proxy")
      where
        adapt (ClientConfig i k v p) = ( i, ( k, ( v, p ) ) )

defaultClientConfig :: ClientConfig
defaultClientConfig
    = ClientConfig { clientID = "", clientKey = "", clientVersion = "169", clientProxy = Nothing }

{-# NOINLINE defaultClientConfig #-}

data Singleton m msg
    = Singleton { clientConfig    :: {-# UNPACK #-} !ClientConfig
                , hathSettings    :: {-# UNPACK #-} !(IORef HathSettings)
                , shutdownRequest :: {-# UNPACK #-} !(MVar ServerAction)
                , logAction       :: !(LogAction m msg)
                , database        :: {-# UNPACK #-} !Connection
                , credential      :: {-# UNPACK #-} !(IORef Credential)
                , galleryTask     :: {-# UNPACK #-} !(Chan ())
                }

instance HasLog (Singleton m msg) msg m where
    getLogAction = logAction

    {-# INLINE getLogAction #-}

    setLogAction l s = s { logAction = l }

    {-# INLINE setLogAction #-}

instance Monad m => MonadReader (Singleton (HathM m) Message) (HathM m) where
    ask = HathM ask

    {-# INLINE ask #-}

    local f (HathM m) = HathM (local f m)

    {-# INLINE local #-}

newtype HathM m a = HathM { runHathM :: ReaderT (Singleton (HathM m) Message) m a }
    deriving newtype ( Functor, Applicative, Monad, MonadThrow, MonadIO, MonadUnliftIO )

runHath :: MonadIO m
        => ClientConfig
        -> IORef HathSettings
        -> Connection
        -> MVar ServerAction
        -> IORef Credential
        -> Chan ()
        -> HathM m a
        -> m a
runHath cfg hRef conn refreshCert credRef queue m = do
    runReaderT
        (runHathM m)
        (Singleton { clientConfig    = cfg
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