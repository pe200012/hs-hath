
module Gallery ( galleryHandler ) where

import           Colog                   ( logInfo, logWarning )

import           Control.Concurrent      ( Chan, readChan )

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import           Data.String.Interpolate ( i )

import           Network.HTTP.Simple     ( getResponseBody, httpLbs )

import           Query

import           Relude

import           Result                  ( RPCResult(..), StatusCode(OK), parseRPCResult )

import           System.Directory        ( createDirectoryIfMissing )

import           Types

import           Utils                   ( hathHash )

galleryHandler :: HathM IO ()
galleryHandler = do
    logInfo "Starting Gallery Downloader..."
    queue <- asks galleryTask
    forever $ do
        void $ liftIO $ readChan queue
        logInfo "Downloading gallery..."
        cfg <- asks clientConfig
        metadata <- liftIO (parseMetadata . getResponseBody <$> galleryMetadata cfg Nothing)
        download cfg metadata

download :: ClientConfig -> GalleryMetadata -> HathM IO ()
download cfg meta = do
    liftIO $ createDirectoryIfMissing True [i|download/#{galleryTitle meta}|]
    mapM_ (fetchFile 0) (galleryFileList meta)
    logInfo "Notify that we have finished downloading..."
    void $ galleryMetadata cfg (Just ( [i|#{gid}|], galleryMinXRes meta ))
  where
    gid = galleryID meta

    fetchFile :: Int -> GalleryFile -> HathM IO ()
    fetchFile trial f
        | trial > 3 = logWarning [i|Failed to download #{galleryFileName f}|]
        | otherwise = do
            res <- parseRPCResult . getResponseBody <$> galleryFetch cfg gid f trial
            case ( rpcStatusCode res, rpcResults res ) of
                ( OK, url : _ ) -> do
                    bytes <- LBS.toStrict . getResponseBody <$> httpLbs [i|#{url}|]
                    if galleryFileHash f == hathHash bytes
                        then do
                            liftIO $ BS.writeFile filePath bytes
                            logInfo [i|Downloaded #{galleryFileName f}.#{galleryFileExt f}|]
                        else fetchFile (trial + 1) f
                _ -> fetchFile (trial + 1) f
      where
        filePath :: FilePath
        filePath = [i|download/#{galleryTitle meta}/#{galleryFileName f}.#{galleryFileExt f}|]