{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils ( module Utils ) where

import           Control.Monad.Loops     ( whileM_ )
import           Control.Monad.ST.Strict ( runST )

import           Crypto.Hash             ( SHA1, hash )

import qualified Data.ByteString.Char8   as BS
import           Data.Default.Class      ( Default(def) )
import qualified Data.Map                as Map
import           Data.STRef.Strict       ( modifySTRef', newSTRef, readSTRef, writeSTRef )

import           Database.SQLite.Simple  ( FromRow, Query, ToRow )
import qualified Database.SQLite.Simple  as SQLite

import           Network.Connection      ( TLSSettings(TLSSettings) )
import           Network.HTTP.Client     ( Manager, newManager )
import           Network.HTTP.Client.TLS ( mkManagerSettings )
import           Network.TLS             ( ClientHooks(..)
                                         , ClientParams(..)
                                         , Credential
                                         , Supported(supportedCiphers)
                                         , defaultParamsClient
                                         )
import           Network.TLS.Extra       ( ciphersuite_strong )

import           Relude

import           Types                   ( HathM, HathSettings, Singleton(..) )

import           Web.Scotty.Trans        ( ActionT, setHeader )

hathHash :: ByteString -> ByteString
hathHash = show . hash @ByteString @SHA1

{-# INLINE hathHash #-}

parseOptions :: ByteString -> Map ByteString ByteString
parseOptions param = runST $ do
    m <- newSTRef Map.empty
    itr <- newSTRef 0
    keyStart <- newSTRef 0
    let withinBound = liftA2 (<) (readSTRef itr) (pure (BS.length param))
    whileM_ withinBound $ do
        readSTRef itr >>= \idx -> case BS.index param idx of
            ';' -> do
                ks <- readSTRef keyStart
                modifySTRef' m $ Map.insert (slice ks idx) ""
                writeSTRef keyStart (idx + 1)
            '=' -> do
                ks <- readSTRef keyStart
                let searchingDelimiter = liftA2 (/=) (BS.index param <$> readSTRef itr) (pure ';')
                whileM_ (liftA2 (&&) withinBound searchingDelimiter) $ modifySTRef' itr succ
                idx2 <- readSTRef itr
                modifySTRef' m $ Map.insert (slice ks idx) (slice (idx + 1) idx2)
                writeSTRef keyStart (idx2 + 1)
            _   -> pure ()

        modifySTRef' itr succ
    readSTRef m
  where
    slice :: Int -> Int -> ByteString
    slice start end = BS.take (end - start) $ BS.drop start param

    {-# INLINE slice #-}

{-# INLINE parseOptions #-}

mkMngr :: String -> Credential -> IO Manager
mkMngr hostName cred = do
    let hooks        = def { onCertificateRequest = const $ return $ Just cred }
        clientParams
            = (defaultParamsClient hostName "")
            { clientHooks     = hooks
            , clientSupported = def { supportedCiphers = ciphersuite_strong }
            }
        tlsSettings  = TLSSettings clientParams
    newManager $ mkManagerSettings tlsSettings Nothing

query :: forall m q r. ( ToRow q, FromRow r, MonadIO m ) => Query -> q -> HathM m [ r ]
query q p = do
    conn <- asks database
    liftIO $ SQLite.query conn q p

{-# INLINE query #-}

query_ :: forall m r. ( FromRow r, MonadIO m ) => Query -> HathM m [ r ]
query_ q = do
    conn <- asks database
    liftIO $ SQLite.query_ conn q

{-# INLINE query_ #-}

execute :: forall m q. ( ToRow q, MonadIO m ) => Query -> q -> HathM m ()
execute q p = do
    conn <- asks database
    liftIO $ SQLite.execute conn q p

{-# INLINE execute #-}

execute_ :: forall m. MonadIO m => Query -> HathM m ()
execute_ q = do
    conn <- asks database
    liftIO $ SQLite.execute_ conn q

{-# INLINE execute_ #-}

getHathSettings :: MonadIO m => HathM m HathSettings
getHathSettings = liftIO . readIORef =<< asks hathSettings

{-# INLINE getHathSettings #-}

commonHeader :: ( IsString a, IsString b ) => [ ( a, b ) ]
commonHeader
    = [ ( "Connection", "close" )
      , ( "User-Agent", "Hentai@Home 169" )
      , ( "Cache-Control", "public, max-age=31536000" )
      , ( "Server", "Genetic Lifeform and Distributed Open Server 1.6.3" )
      , ( "X-Content-Type-Options", "nosniff" )
      ]

{-# INLINE commonHeader #-}

setCommonHeader :: MonadIO m => ActionT m ()
setCommonHeader = mapM_ (uncurry setHeader) commonHeader

{-# INLINE setCommonHeader #-}
