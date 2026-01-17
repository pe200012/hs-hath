{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module R2
  ( R2Connection(..)
  , mkR2Connection
  , runCacheR2
  ) where

import           Colog                   ( Message, Severity(Error, Warning) )
import           Colog.Polysemy          ( Log )

import qualified Conduit                 as C
import           Control.Exception       ( SomeException, try )

import qualified Data.ByteArray          as BA
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import           Data.String             ( IsString(fromString) )
import qualified Data.Text               as T

import           Database                ( FileRecord(..) )

import           Network.Minio           ( CredentialValue(..)
                                         , AccessKey(..)
                                         , SecretKey(..)
                                         , setCreds
                                         , setRegion
                                         , defaultGetObjectOptions
                                         , defaultPutObjectOptions
                                         , getObject
                                         , removeObject
                                         , runMinio
                                         )
import qualified Network.Minio           as Minio

import           Polysemy
import           Polysemy.Error          ( Error, throw )
import           Polysemy.KVStore        ( KVStore(..) )
import           Polysemy.Operators

import           Relude                  hiding ( Reader, ask )

import           Types                   ( FileURI(..), RPCError(..) )
import qualified Types                   as Ty

import           Utils                   ( log )

-- | R2 connection configuration built at runtime
data R2Connection
  = R2Connection { r2ConnInfo   :: !Minio.ConnectInfo
                 , r2ConnBucket :: !Text
                 }

-- | Build R2 connection from config + environment variables
-- Reads R2_ACCESS_KEY and R2_SECRET_KEY from environment
mkR2Connection :: Ty.R2Config -> IO (Either Text R2Connection)
mkR2Connection cfg = do
  accessKeyMay <- lookupEnv "R2_ACCESS_KEY"
  secretKeyMay <- lookupEnv "R2_SECRET_KEY"
  case (accessKeyMay, secretKeyMay) of
    (Nothing, _) -> pure $ Left "R2_ACCESS_KEY environment variable not set"
    (_, Nothing) -> pure $ Left "R2_SECRET_KEY environment variable not set"
    (Just accessKey, Just secretKey) -> do
      let creds = CredentialValue
            { cvAccessKey    = AccessKey $ T.pack accessKey
            , cvSecretKey    = SecretKey $ BA.convert (encodeUtf8 @String @ByteString secretKey)
            , cvSessionToken = Nothing
            }
          -- Use IsString instance to parse endpoint URL
          baseConnInfo = fromString $ T.unpack $ Ty.r2Endpoint cfg
          -- Set credentials and region
          connInfo = setCreds creds $ setRegion "auto" baseConnInfo
      pure $ Right R2Connection
        { r2ConnInfo   = connInfo
        , r2ConnBucket = Ty.r2Bucket cfg
        }

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
reconstructRecord uri bytes = FileRecord
  { fileRecordLRUCounter = 0
  , fileRecordS4         = T.take 4 (show uri)
  , fileRecordFileId     = show uri
  , fileRecordFileName   = Nothing
  , fileRecordBytes      = bytes
  }

-- | Run the cache with Cloudflare R2
-- Read operations silently degrade to Nothing on failure (with warning log)
-- Write/delete operations propagate errors
runCacheR2
  :: Members '[Embed IO, Log Message, Error RPCError] r
  => R2Connection
  -> KVStore FileURI FileRecord : r @> a
  -> r @> a
runCacheR2 conn = interpret $ \case
  LookupKV uri -> do
    let key    = fileURIToKey uri
        bucket = r2ConnBucket conn
    result <- embed $ try @SomeException $ runMinio (r2ConnInfo conn) $ do
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
      Right (Right bytes) ->
        pure $ Just $ reconstructRecord uri bytes

  UpdateKV uri (Just record) -> do
    let key     = fileURIToKey uri
        bucket  = r2ConnBucket conn
        content = fileRecordBytes record
        size    = fromIntegral (BS.length content) :: Int64
        src     = C.yield content
    result <- embed $ try @SomeException $ runMinio (r2ConnInfo conn) $
      Minio.putObject bucket key src (Just size) defaultPutObjectOptions
    case result of
      Left err -> do
        log Error ("R2 write failed for " <> key <> ": " <> T.pack (show err))
        throw $ Ty.R2WriteError (T.pack $ show err)
      Right (Left minioErr) -> do
        log Error ("R2 write failed for " <> key <> ": " <> T.pack (show minioErr))
        throw $ Ty.R2WriteError (T.pack $ show minioErr)
      Right (Right _) -> pure ()

  UpdateKV uri Nothing -> do
    let key    = fileURIToKey uri
        bucket = r2ConnBucket conn
    result <- embed $ try @SomeException $ runMinio (r2ConnInfo conn) $
      removeObject bucket key
    case result of
      Left err -> do
        log Error ("R2 delete failed for " <> key <> ": " <> T.pack (show err))
        throw $ Ty.R2WriteError (T.pack $ show err)
      Right (Left minioErr) -> do
        log Error ("R2 delete failed for " <> key <> ": " <> T.pack (show minioErr))
        throw $ Ty.R2WriteError (T.pack $ show minioErr)
      Right (Right _) -> pure ()
