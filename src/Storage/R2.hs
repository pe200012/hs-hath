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
import qualified Data.Map                as Map
import qualified Data.Text               as T

import           Network.HTTP.Client     ( newManager )
import           Network.HTTP.Client.TLS ( tlsManagerSettings )
import           Network.Minio           ( AccessKey(..)
                                         , CredentialValue(..)
                                         , SecretKey(..)
                                         , defaultGetObjectOptions
                                         , defaultPutObjectOptions
                                         , getObject
                                         , removeObject
                                         , setCreds
                                         , setRegion
                                         )
import qualified Network.Minio           as Minio

import           Polysemy
import           Polysemy.Error          ( Error, throw )
import           Polysemy.KVStore        ( KVStore(..) )
import           Polysemy.Operators

import           Relude                  hiding ( Reader, ask )

import           Storage.Database        ( FileRecord(..) )

import           System.IO.Unsafe        ( unsafePerformIO )

import           Types                   ( FileURI(..), RPCError(..) )
import qualified Types                   as Ty

import           Utils                   ( log )

memoryCache :: IORef (Map FileURI FileRecord)
memoryCache = unsafePerformIO (newIORef mempty)

{-# NOINLINE memoryCache #-}

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

-- | Reconstruct FileRecord from FileURI and bytes
-- Metadata fields are set to defaults since R2 only stores bytes
reconstructRecord :: FileURI -> ByteString -> FileRecord
reconstructRecord uri bytes
  = FileRecord { fileRecordLRUCounter = 0
               , fileRecordS4         = T.take 4 (show uri)
               , fileRecordFileId     = show uri
               , fileRecordFileName   = Nothing
               , fileRecordBytes      = bytes
               }

-- | Run the cache with Cloudflare R2
-- Read operations silently degrade to Nothing on failure (with warning log)
-- Write/delete operations propagate errors
runCacheR2 :: Members '[ Embed IO, Log Message, Error RPCError ] r
           => R2Connection
           -> KVStore FileURI FileRecord : r @> a
           -> r @> a
runCacheR2 conn = interpret $ \case
  LookupKV uri -> do
    let key    = fileURIToKey uri
        bucket = r2ConnBucket conn
    memCache <- readIORef memoryCache
    case Map.lookup uri memCache of
      Just res -> pure $ Just res
      Nothing  -> do
        result <- embed $ try @SomeException $ Minio.runMinioWith (r2MinioConn conn) $ do
          resp <- getObject bucket key defaultGetObjectOptions
          -- Consume the response body
          let src = Minio.gorObjectStream resp
          lbs <- C.runConduit $ src C..| C.sinkLazy
          pure $ BL.toStrict lbs
        case result of
          Left err -> do
            log Warning ("R2 lookup failed for " <> key <> ": " <> T.pack (show err))
            pure Nothing
          Right (Left minioErr) -> do
            log Warning ("R2 lookup failed for " <> key <> ": " <> T.pack (show minioErr))
            pure Nothing
          Right (Right bytes) -> do
            let recd = reconstructRecord uri bytes
            if Map.size memCache > 100
              then writeIORef memoryCache (Map.singleton uri recd)
              else writeIORef memoryCache (Map.insert uri recd memCache)
            pure $ Just recd

  UpdateKV uri (Just record) -> do
    let key     = fileURIToKey uri
        bucket  = r2ConnBucket conn
        content = fileRecordBytes record
        size    = fromIntegral (BS.length content) :: Int64
        src     = C.yield content
    result <- embed
      $ try @SomeException
      $ Minio.runMinioWith (r2MinioConn conn)
      $ Minio.putObject bucket key src (Just size) defaultPutObjectOptions
    case result of
      Left err -> do
        log Error ("R2 write failed for " <> key <> ": " <> T.pack (show err))
        throw $ Ty.R2WriteError (T.pack $ show err)
      Right (Left minioErr) -> do
        log Error ("R2 write failed for " <> key <> ": " <> T.pack (show minioErr))
        throw $ Ty.R2WriteError (T.pack $ show minioErr)
      Right (Right _) -> modifyIORef' memoryCache (Map.alter (const (Just record)) uri)

  UpdateKV uri Nothing -> do
    let key    = fileURIToKey uri
        bucket = r2ConnBucket conn
    result <- embed
      $ try @SomeException
      $ Minio.runMinioWith (r2MinioConn conn)
      $ removeObject bucket key
    case result of
      Left err -> do
        log Error ("R2 delete failed for " <> key <> ": " <> T.pack (show err))
        throw $ Ty.R2WriteError (T.pack $ show err)
      Right (Left minioErr) -> do
        log Error ("R2 delete failed for " <> key <> ": " <> T.pack (show minioErr))
        throw $ Ty.R2WriteError (T.pack $ show minioErr)
      Right (Right _) -> modifyIORef' memoryCache (Map.delete uri)
