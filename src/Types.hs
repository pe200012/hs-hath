{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Types ( module Types ) where

import           Colog                  ( HasLog, LogAction, Message, richMessageAction )
import           Colog.Core.Class       ( HasLog(..) )

import           Control.Concurrent     ( MVar )
import           Control.Exception      ( Exception )
import           Control.Monad.Catch    ( MonadThrow )
import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Reader   ( MonadReader(..), ReaderT(runReaderT) )

import           Data.Bifunctor         ( Bifunctor(bimap) )
import           Data.ByteString        ( ByteString )
import           Data.HashSet           ( HashSet )
import           Data.IORef             ( IORef )
import           Data.Int               ( Int64 )
import           Data.Text              ( Text )
import           Data.Text.Encoding     ( decodeUtf8, encodeUtf8 )

import           Database.SQLite.Simple ( Connection )

import           Dhall                  ( FromDhall, Generic, ToDhall )

import           Network.TLS            ( Credential )

import           UnliftIO               ( MonadUnliftIO )

data HathError = InitialContactFailure | InvalidClientKey | InvalidCertificate
    deriving ( Show )

newtype RefreshCert = RefreshCert Credential

data GracefulShutdown = GracefulShutdown
    deriving ( Show )

instance Exception GracefulShutdown

data HathSettings
    = HathSettings
    { rpcBaseURL         :: {-# UNPACK #-} !ByteString
    , clientHost         :: {-# UNPACK #-} !ByteString
    , clientPort         :: {-# UNPACK #-} !Int
    , throttleBytes      :: {-# UNPACK #-} !Int
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

data ClientProxy
    = ClientProxy { proxyHost :: {-# UNPACK #-} !ByteString
                  , proxyPort :: {-# UNPACK #-} !Int
                  , proxyAuth :: {-# UNPACK #-} !(Maybe ( ByteString, ByteString ))
                  }

data SerializedClientProxy
    = SerializedClientProxy { sProxyHost :: {-# UNPACK #-} !Text
                            , sProxyPort :: {-# UNPACK #-} !Int
                            , sProxyAuth :: {-# UNPACK #-} !(Maybe ( Text, Text ))
                            }
    deriving ( Generic )

instance FromDhall SerializedClientProxy

instance ToDhall SerializedClientProxy

marshallClientProxy :: ClientProxy -> SerializedClientProxy
marshallClientProxy (ClientProxy h p pa)
    = SerializedClientProxy (decodeUtf8 h) p (bimap decodeUtf8 decodeUtf8 <$> pa)

{-# INLINE marshallClientProxy #-}

unmarshallClientProxy :: SerializedClientProxy -> ClientProxy
unmarshallClientProxy (SerializedClientProxy h p pa)
    = ClientProxy (encodeUtf8 h) p (bimap encodeUtf8 encodeUtf8 <$> pa)

{-# INLINE unmarshallClientProxy #-}

data ClientConfig
    = ClientConfig { clientID      :: {-# UNPACK #-} !ByteString
                   , clientKey     :: {-# UNPACK #-} !ByteString
                   , clientVersion :: {-# UNPACK #-} !ByteString
                   , clientProxy   :: {-# UNPACK #-} !(Maybe ClientProxy)
                   }

defaultClientConfig :: ClientConfig
defaultClientConfig
    = ClientConfig { clientID = "", clientKey = "", clientVersion = "160", clientProxy = Nothing }

{-# NOINLINE defaultClientConfig #-}

data SerializedClientConfig
    = SerializedClientConfig { sClientID      :: {-# UNPACK #-} !Text
                             , sClientKey     :: {-# UNPACK #-} !Text
                             , sClientVersion :: {-# UNPACK #-} !Text
                             , sClientProxy   :: {-# UNPACK #-} !(Maybe SerializedClientProxy)
                             }
    deriving ( Generic )

instance FromDhall SerializedClientConfig

instance ToDhall SerializedClientConfig

marshallClientConfig :: ClientConfig -> SerializedClientConfig
marshallClientConfig (ClientConfig cid ckey cver cproxy)
    = SerializedClientConfig
        (decodeUtf8 cid)
        (decodeUtf8 ckey)
        (decodeUtf8 cver)
        (marshallClientProxy <$> cproxy)

{-# INLINE marshallClientConfig #-}

unmarshallClientConfig :: SerializedClientConfig -> ClientConfig
unmarshallClientConfig (SerializedClientConfig cid ckey cver cproxy)
    = ClientConfig
        (encodeUtf8 cid)
        (encodeUtf8 ckey)
        (encodeUtf8 cver)
        (unmarshallClientProxy <$> cproxy)

{-# INLINE unmarshallClientConfig #-}

data Singleton m msg
    = Singleton { clientConfig       :: {-# UNPACK #-} !ClientConfig
                , hathSettings       :: {-# UNPACK #-} !(IORef HathSettings)
                , refreshCertificate :: {-# UNPACK #-} !(MVar RefreshCert)
                , logAction          :: !(LogAction m msg)
                , database           :: {-# UNPACK #-} !Connection
                , credential         :: {-# UNPACK #-} !(IORef Credential)
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
        -> MVar RefreshCert
        -> IORef Credential
        -> HathM m a
        -> m a
runHath cfg hRef conn refreshCert credRef m = do
    runReaderT
        (runHathM m)
        (Singleton { clientConfig       = cfg
                   , hathSettings       = hRef
                   , refreshCertificate = refreshCert
                   , logAction          = richMessageAction
                   , database           = conn
                   , credential         = credRef
                   })

{-# INLINE runHath #-}

