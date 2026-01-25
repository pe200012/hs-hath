{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Storage.R2 ( R2Connection(..), mkR2Connection, runCacheR2 ) where

import           Colog                   ( Message, Severity(Error, Warning) )
import           Colog.Polysemy          ( Log )

import qualified Conduit                 as C

import           Control.Exception       ( try )

import qualified Data.ByteArray          as BA
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import           Data.Cache.LRU.IO
import qualified Data.Cache.LRU.IO       as LRU
import qualified Data.Text               as T

import           Network.HTTP.Client     ( newManager )
import           Network.HTTP.Client.TLS ( tlsManagerSettings )
import           Network.Minio           ( AccessKey(..)
                                         , CredentialValue(..)
                                         , SecretKey(..)
                                         , defaultGetObjectOptions
                                         , defaultPutObjectOptions
                                         , getObject
                                         , presignedGetObjectUrl
                                         , removeObject
                                         , setCreds
                                         , setRegion
                                         )
import qualified Network.Minio           as Minio

import           Polysemy
import           Polysemy.Error          ( Error, throw )
import           Polysemy.KVStore        ( KVStore(..) )
import           Polysemy.Operators
import           Polysemy.Reader         ( Reader, asks )

import           Relude                  hiding ( Reader, ask, asks )

import           Storage.Database        ( FileRecord(..) )

import           Types                   ( ClientConfig
                                         , FileURI(..)
                                         , RPCError(..)
                                         , StorageResult(..)
                                         )
import qualified Types                   as Ty

import           Utils                   ( log )

-- | R2 connection configuration built at runtime
data R2Connection = R2Connection { r2MinioConn :: !Minio.MinioConn, r2ConnBucket :: !Text }

-- | Build R2 connection from config + environment variables
-- Reads R2_ACCESS_KEY and R2_SECRET_KEY from environment
mkR2Connection :: Ty.R2Config -> IO (Either Text R2Connection)
mkR2Connection cfg = do
  accessKeyMay <- lookupEnv "R2_ACCESS_KEY"
  secretKeyMay <- lookupEnv "R2_SECRET_KEY"
  case ( accessKeyMay, secretKeyMay ) of
    ( Nothing, _ ) -> pure $ Left "R2_ACCESS_KEY environment variable not set"
    ( _, Nothing ) -> pure $ Left "R2_SECRET_KEY environment variable not set"
    ( Just accessKey, Just secretKey ) -> do
      let creds
            = CredentialValue
            { cvAccessKey    = AccessKey $ T.pack accessKey
            , cvSecretKey    = SecretKey $ BA.convert (encodeUtf8 @String @ByteString secretKey)
            , cvSessionToken = Nothing
            }
          -- Use IsString instance to parse endpoint URL
          baseConnInfo = fromString $ T.unpack $ Ty.r2Endpoint cfg
      -- Set credentials and region
      minioConn <- liftIO $ do
        manager <- newManager tlsManagerSettings
        Minio.mkMinioConn (setCreds creds $ setRegion "auto" baseConnInfo) manager
      pure $ Right R2Connection { r2MinioConn = minioConn, r2ConnBucket = Ty.r2Bucket cfg }

-- | Generate R2 object key from FileURI
-- Format: s4/hash-size-xres-yres-ext
fileURIToKey :: FileURI -> Text
fileURIToKey uri = s4 <> "/" <> fileId
  where
    fileId = show uri

    s4     = T.take 4 fileId

-- | Run the cache with Cloudflare R2
-- Read operations silently degrade to Nothing on failure (with warning log)
-- Write/delete operations propagate errors
runCacheR2 :: Members '[ Embed IO, Log Message, Reader ClientConfig, Error RPCError ] r
           => R2Connection
           -> KVStore FileURI StorageResult : r @> a
           -> r @> a
runCacheR2 conn m = do
  lruSize <- asks Ty.lruCacheSize
  let entries = Just (fromIntegral (lruSize `div` 300000)) -- Approximate number of entries based on avg file size ~300 KiB
  memCache <- embed $ newAtomicLRU entries
  interpret (phi memCache) m
  where
    bucket = r2ConnBucket conn

    phi :: forall r r1 x. Members '[ Embed IO, Log Message, Error RPCError ] r1
        => AtomicLRU FileURI FileRecord
        -> KVStore FileURI StorageResult (Sem r) x
        -> Sem r1 x
    phi cache (LookupKV uri@(fileURIToKey -> key))         = do
      embed (LRU.lookup uri cache) >>= \case
        Just res -> pure $ Just $ Record res
        Nothing  -> do
          -- Try to generate presigned URL (60 seconds expiry)
          presignedResult <- embed
            $ try @SomeException
            $ Minio.runMinioWith (r2MinioConn conn)
            $ presignedGetObjectUrl bucket key 60 [] []
          case presignedResult of
            Right (Right url) -> do
              -- Successfully generated presigned URL
              pure $ Just (Redirect url)
            _ -> do
              -- Fallback: presigned URL generation failed, download file instead
              log
                Warning
                ("Presigned URL generation failed for " <> key <> ", falling back to proxy mode")
              downloadResult
                <- embed $ try @SomeException $ Minio.runMinioWith (r2MinioConn conn) $ do
                  resp <- getObject bucket key defaultGetObjectOptions
                  -- Consume the response body
                  let src = Minio.gorObjectStream resp
                  lbs <- C.runConduit $ src C..| C.sinkLazy
                  pure $ BL.toStrict lbs
              case downloadResult of
                Left err -> do
                  log Warning ("R2 download failed for " <> key <> ": " <> T.pack (show err))
                  pure Nothing
                Right (Left minioErr) -> do
                  log Warning ("R2 download failed for " <> key <> ": " <> T.pack (show minioErr))
                  pure Nothing
                Right (Right (Ty.reconstructRecord uri -> record)) -> do
                  -- Cache the downloaded file
                  embed $ LRU.insert uri record cache
                  pure $ Just (Record record)

    phi _cache (UpdateKV _uri (Just (Redirect _url)))
      = error "impossible: cannot store Redirect in R2 backend"

    phi cache (UpdateKV uri (Just (Record record)))        = do
      let key     = fileURIToKey uri
          content = fileRecordBytes record
          siz     = fromIntegral (BS.length content) :: Int64
          src     = C.yield content
      result <- embed
        $ try @SomeException
        $ Minio.runMinioWith (r2MinioConn conn)
        $ Minio.putObject bucket key src (Just siz) defaultPutObjectOptions
      case result of
        Left err -> do
          log Error ("R2 write failed for " <> key <> ": " <> T.pack (show err))
          throw $ Ty.StorageError (T.pack $ show err)
        Right (Left minioErr) -> do
          log Error ("R2 write failed for " <> key <> ": " <> T.pack (show minioErr))
          throw $ Ty.StorageError (T.pack $ show minioErr)
        Right (Right _) -> embed $ LRU.insert uri record cache

    phi cache (UpdateKV uri@(fileURIToKey -> key) Nothing) = do
      result <- embed
        $ try @SomeException
        $ Minio.runMinioWith (r2MinioConn conn)
        $ removeObject bucket key
      case result of
        Left err -> do
          log Error ("R2 delete failed for " <> key <> ": " <> T.pack (show err))
          throw $ Ty.StorageError (T.pack $ show err)
        Right (Left minioErr) -> do
          log Error ("R2 delete failed for " <> key <> ": " <> T.pack (show minioErr))
          throw $ Ty.StorageError (T.pack $ show minioErr)
        Right (Right _) -> embed $ void $ LRU.delete uri cache
