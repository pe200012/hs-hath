{-# LANGUAGE DataKinds #-}

-- | The gallery downloader: fetches the galleries queued for this client on the
-- H\@H site into the download directory.
module Hath.Gallery.Downloader ( GalleryTask(..), runGalleryDownloader, galleryFilePath ) where

import           Colog                   ( Message, richMessageAction )
import           Colog.Polysemy          ( Log, runLogAction )

import           Control.Exception       ( try )

import qualified Data.ByteString.Char8   as BS
import           Data.String.Interpolate ( i )

import           Hath.AtomicFile         ( writeFileAtomic )
import           Hath.Config             ( ClientConfig(..) )
import           Hath.Error              ( HathError )
import           Hath.Gallery            ( GalleryFile(..), GalleryMetadata(..) )
import           Hath.Log                ( logInfo, logWarning )
import           Hath.Protocol           ( sha1Hex )
import           Hath.RPC                ( RPC
                                         , completeGallery
                                         , galleryFileSources
                                         , nextGallery
                                         , reportDownloadFailures
                                         , runRPC
                                         )
import           Hath.Stats              ( StatsEnv, addDlBytes, incDlFile, incDlTask )

import           Network.HTTP.Client     ( HttpException
                                         , Request(host)
                                         , httpLbs
                                         , parseRequest
                                         , responseBody
                                         )
import           Network.HTTP.Client.TLS ( getGlobalManager )

import           Polysemy
import           Polysemy.Error          ( Error, errorToIOFinal )
import           Polysemy.Reader         ( Reader, ask, runReader )
import           Polysemy.State          ( modify, runState )

import           Relude                  hiding ( Reader, ask, modify, runReader, runState )

import           System.Directory        ( doesFileExist )
import           System.FilePath         ( (<.>), (</>) )
import           System.IO               ( hPutStrLn )

import           UnliftIO                ( TChan, readTChan )

-- | A request to look at the download queue (the server's @start_downloader@).
data GalleryTask = GalleryTask

-- | Serve 'GalleryTask's forever, one gallery per task. A failed download is
-- logged; the server will ask again.
runGalleryDownloader :: ClientConfig -> StatsEnv -> TChan GalleryTask -> IO Void
runGalleryDownloader config stats tasks = infinitely $ do
  GalleryTask <- atomically $ readTChan tasks
  outcome <- try @SomeException
    $ runFinal
    $ embedToFinal @IO
    $ errorToIOFinal @HathError
    $ runReader config
    $ runReader stats
    $ runLogAction @IO @Message richMessageAction
    $ runRPC downloadNextGallery
  case outcome of
    Left e           -> hPutStrLn stderr $ "Gallery downloader error: " <> show e
    Right (Left e)   -> hPutStrLn stderr $ "Gallery downloader error: " <> show e
    Right (Right ()) -> pass

-- | Download the next queued gallery, if there is one, and report it complete
-- once every file is on disk.
downloadNextGallery
  :: Members
    '[ RPC, Reader StatsEnv, Log Message, Reader ClientConfig, Error HathError, Embed IO ]
    r
  => Sem r ()
downloadNextGallery = do
  queued <- nextGallery
  incDlTask
  case queued of
    Left "NO_PENDING_DOWNLOADS" -> logInfo "No gallery task available"
    Left reason -> logWarning [i|Gallery download refused: #{reason}|]
    Right gallery -> do
      logInfo [i|Starting download for gallery #{galleryID gallery}|]
      directory <- toString . downloadDir <$> ask
      complete <- mapM (ensureFile directory gallery) (galleryFileList gallery)
      when (and complete) (completeGallery gallery)

-- | Where a gallery file goes: @\<download directory>\/\<gallery id>\/\<name>.\<type>@.
-- 'Hath.Gallery.parseMetadata' has made sure the name and type are plain.
galleryFilePath :: FilePath -> GalleryMetadata -> GalleryFile -> FilePath
galleryFilePath directory gallery file
  = directory
  </> show (galleryID gallery)
  </> (text (galleryFileName file) <.> text (galleryFileExt file))
  where
    text = toString . decodeUtf8 @Text

-- | Whether the file is on disk afterwards: it was already there and intact,
-- or it has just been downloaded.
ensureFile :: Members '[ RPC, Reader StatsEnv, Log Message, Error HathError, Embed IO ] r
           => FilePath
           -> GalleryMetadata
           -> GalleryFile
           -> Sem r Bool
ensureFile directory gallery file = do
  existing <- embed $ do
    exists <- doesFileExist path
    if exists
      then BS.readFile path
      else pure BS.empty
  if not (BS.null existing) && hasExpectedHash file existing
    then do
      logInfo [i|#{name} already verified, skipping|]
      pure True
    else downloadGalleryFile gallery file >>= \case
      Nothing    -> do
        logWarning [i|Failed to download #{name}|]
        pure False
      Just bytes -> do
        logInfo [i|Downloaded #{name}|]
        incDlFile
        addDlBytes (BS.length bytes)
        embed $ writeFileAtomic path bytes
        pure True
  where
    name :: String
    name = [i|#{galleryFileName file}.#{galleryFileExt file}|]

    path = galleryFilePath directory gallery file

-- | A file generated on demand has no known hash; like the official client, accept it.
hasExpectedHash :: GalleryFile -> ByteString -> Bool
hasExpectedHash file content = maybe True (== sha1Hex content) (galleryFileHash file)

-- | Try the sources the server names, asking for new ones up to three more
-- times. Sources that serve the wrong content are reported if we end up with nothing.
downloadGalleryFile :: Members '[ RPC, Error HathError, Embed IO ] r
                    => GalleryMetadata
                    -> GalleryFile
                    -> Sem r (Maybe ByteString)
downloadGalleryFile gallery file = do
  ( failures, content ) <- runState @[ Text ] [] (attempt 0)
  when (isNothing content) $ reportDownloadFailures failures
  pure content
  where
    attempt (retries :: Int)
      | retries > 3 = pure Nothing
      | otherwise = do
        urls <- galleryFileSources gallery file retries
        -- an empty list is probably a network hiccup; either way, ask again
        runMaybeT (asum (MaybeT . download . BS.unpack <$> urls))
          >>= maybe (attempt (retries + 1)) (pure . Just)

    download url = case parseRequest url of
      Nothing  -> pure Nothing
      Just req -> embed
        (try @HttpException $ toStrict . responseBody <$> (httpLbs req =<< getGlobalManager))
        >>= \case
          Left _      -> pure Nothing
          Right bytes
            | hasExpectedHash file bytes -> pure (Just bytes)
            | otherwise -> do
              modify
                @[ Text ]
                ([i|#{host req}-#{galleryFileIndex file}-#{galleryFileXRes file}|] :)
              pure Nothing
