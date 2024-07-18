
module Gallery ( galleryHandler ) where

import           Colog                   ( logInfo, logWarning )

import           Control.Concurrent      ( readChan )

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import           Data.String.Interpolate ( i )

import           Network.HTTP.Simple     ( getResponseBody, httpLbs )

import           Query

import           Relude

import           Result                  ( RPCResult(..), StatusCode(OK), parseRPCResult )

import           System.Directory        ( createDirectoryIfMissing )

import           Types

import           UnliftIO                ( tryAny )
import           UnliftIO.Directory      ( doesFileExist )

import           Utils                   ( hathHash )

galleryHandler :: HathM IO ()
galleryHandler = do
    logInfo "Starting Gallery Listener..."
    queue <- asks galleryTask
    forever $ void $ tryAny $ do
        void $ liftIO $ readChan queue
        logInfo "Downloading gallery..."
        cfg <- asks clientConfig
        metadata <- liftIO (parseMetadata . getResponseBody <$> galleryMetadata cfg Nothing)
        download cfg metadata

download :: ClientConfig -> GalleryMetadata -> HathM IO ()
download cfg meta = do
    liftIO $ createDirectoryIfMissing True [i|download/#{galleryTitle meta}|]
    mapM_ checkAndFetch (galleryFileList meta)
    logInfo "Notify that we have finished downloading..."
    void $ galleryMetadata cfg (Just ( [i|#{gid}|], galleryMinXRes meta ))
  where
    gid = galleryID meta

    checkAndFetch f = doesFileExist filePath >>= \case
        True  -> do
            existingBytes <- liftIO $ BS.readFile filePath
            if galleryFileHash f == hathHash existingBytes
                then logInfo [i|#{galleryFileName f}.#{galleryFileExt f} already exists, skipping|]
                else operate 0
        False -> operate 0
      where
        operate trial
            | trial > 3 = logWarning [i|Failed to download #{galleryFileName f}|]
            | otherwise = tryAny (fetch trial) >>= \case
                Right True -> pure ()
                _          -> operate (trial + 1)

        fetch :: Int -> HathM IO Bool
        fetch trial = do
            res <- parseRPCResult . getResponseBody <$> galleryFetch cfg gid f trial
            case ( rpcStatusCode res, rpcResults res ) of
                ( OK, url : _ ) -> do
                    bytes <- LBS.toStrict . getResponseBody <$> httpLbs [i|#{url}|]
                    if galleryFileHash f == hathHash bytes
                        then do
                            liftIO $ BS.writeFile filePath bytes
                            logInfo [i|Downloaded #{galleryFileName f}.#{galleryFileExt f}|]
                            pure True
                        else pure False
                _ -> pure False

        filePath = [i|download/#{galleryTitle meta}/#{galleryFileName f}.#{galleryFileExt f}|]