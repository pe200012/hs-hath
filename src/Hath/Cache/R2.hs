{-# LANGUAGE DataKinds #-}

-- | The R2 backend (any S3-compatible store): hits are answered with a redirect
-- to a presigned link, so the reader fetches the file from the bucket itself.
module Hath.Cache.R2 ( R2Connection(..), mkR2Connection, runCacheR2, runR2, fileURIToKey ) where

import           Colog                   ( Message )
import           Colog.Polysemy          ( Log )

import qualified Conduit                 as C

import           Control.Exception       ( try )

import qualified Data.ByteArray          as BA
import qualified Data.ByteString         as BS
import           Data.Cache.LRU.IO       ( AtomicLRU, newAtomicLRU )
import qualified Data.Cache.LRU.IO       as LRU
import qualified Data.Text               as T

import           GHC.Clock               ( getMonotonicTime )

import           Hath.Cache              ( Cache(..), CacheHit(..) )
import           Hath.Config             ( R2Config(..) )
import           Hath.Error              ( HathError(..) )
import           Hath.FileURI            ( FileURI, renderFileURIText )
import           Hath.Log                ( logError, logWarning )

import           Network.HTTP.Client     ( newManager )
import           Network.HTTP.Client.TLS ( tlsManagerSettings )
import           Network.Minio           ( AccessKey(..)
                                         , CredentialValue(..)
                                         , SecretKey(..)
                                         , defaultPutObjectOptions
                                         , presignedGetObjectUrl
                                         , setCreds
                                         , setRegion
                                         )
import qualified Network.Minio           as Minio
import           Network.Minio.S3API     ( headObject )

import           Polysemy
import           Polysemy.Error          ( Error, throw )

import           Relude

data R2Connection
  = R2Connection
  { r2MinioConn  :: !Minio.MinioConn
  , r2ConnBucket :: !Text
  , r2UrlCache   :: !(AtomicLRU FileURI ( Double, ByteString ))
    -- ^ Presigned links and when we stop handing them out (monotonic seconds).
    -- It lives here because the interpreter runs once per request.
  }

-- | Connect with the endpoint and bucket from the config and the credentials
-- from @R2_ACCESS_KEY@ and @R2_SECRET_KEY@.
mkR2Connection :: R2Config -> IO (Either Text R2Connection)
mkR2Connection cfg = do
  accessKeyMay <- lookupEnv "R2_ACCESS_KEY"
  secretKeyMay <- lookupEnv "R2_SECRET_KEY"
  case ( accessKeyMay, secretKeyMay ) of
    ( Nothing, _ ) -> pure $ Left "R2_ACCESS_KEY environment variable not set"
    ( _, Nothing ) -> pure $ Left "R2_SECRET_KEY environment variable not set"
    ( Just accessKey, Just secretKey ) -> do
      let creds
            = CredentialValue
            { cvAccessKey    = AccessKey $ toText accessKey
            , cvSecretKey    = SecretKey $ BA.convert (encodeUtf8 @String @ByteString secretKey)
            , cvSessionToken = Nothing
            }
          -- the IsString instance parses the endpoint URL
          baseConnInfo = fromString $ toString $ r2Endpoint cfg
      manager <- newManager tlsManagerSettings
      minioConn <- Minio.mkMinioConn (setCreds creds $ setRegion "auto" baseConnInfo) manager
      -- ponytail: fixed capacity; links expire after 10 min, so it only has to hold ~10 min of distinct hits
      urlCache <- newAtomicLRU (Just 100000)
      pure
        $ Right
          R2Connection
          { r2MinioConn = minioConn, r2ConnBucket = r2Bucket cfg, r2UrlCache = urlCache }

-- | R2 object key for a file: @abcd\/\<file id>@, abcd being the first 4 hex digits
-- of the hash. Every client sharing the bucket must use this layout (the Java
-- fork's S3CacheBackend.objectKey does).
fileURIToKey :: FileURI -> Text
fileURIToKey uri = T.take 4 fileId <> "/" <> fileId
  where
    fileId = renderFileURIText uri

-- | Run a bucket operation. Exceptions and Minio's own errors alike come back as text.
runR2 :: R2Connection -> Minio.Minio a -> IO (Either Text a)
runR2 conn action = try @SomeException (Minio.runMinioWith (r2MinioConn conn) action) <&> \case
  Left err -> Left (show err)
  Right (Left err) -> Left (show err)
  Right (Right a) -> Right a

-- | A lookup that fails for any reason is a miss (with a warning); a failed
-- write is an error.
runCacheR2
  :: Members '[ Embed IO, Log Message, Error HathError ] r => R2Connection -> Sem (Cache : r) a -> Sem r a
runCacheR2 conn = interpret $ \case
  LookupFile uri -> do
    let objectKey = fileURIToKey uri
    now <- embed getMonotonicTime
    embed (LRU.lookup uri (r2UrlCache conn)) >>= \case
      Just ( expiry, link )
        | now < expiry -> pure $ Just $ Redirect link
      _ -> do
        -- ask the bucket whether it has the object before sending anyone there
        found <- embed $ minio $ headObject bucket objectKey []
        case found of
          Right _ -> fmap Redirect <$> embed (rememberLink uri now)
          Left _  -> do
            logWarning ("Object not found in R2 storage: " <> objectKey)
            pure Nothing

  StoreFile uri bytes -> do
    let objectKey = fileURIToKey uri
        size      = fromIntegral (BS.length bytes) :: Int64
    stored <- embed
      $ minio
      $ Minio.putObject bucket objectKey (C.yield bytes) (Just size) defaultPutObjectOptions
    case stored of
      Left err -> do
        logError ("R2 write failed for " <> objectKey <> ": " <> err)
        throw $ StorageError err
      -- it is there now, as after a HEAD: the next hit (often seconds away) needs none
      Right () -> embed $ getMonotonicTime >>= void . rememberLink uri
  where
    bucket = r2ConnBucket conn

    minio :: Minio.Minio a -> IO (Either Text a)
    minio = runR2 conn

    -- Sign a link (local, no request) and reuse it for 10 min.
    rememberLink :: FileURI -> Double -> IO (Maybe ByteString)
    rememberLink uri now
      -- valid 20 min, handed out for 10, so every link given lasts at least 10 min
      = minio (presignedGetObjectUrl bucket (fileURIToKey uri) 1200 [] []) >>= \case
        Left _     -> pure Nothing
        Right link -> do
          LRU.insert uri ( now + 600, link ) (r2UrlCache conn)
          pure (Just link)
