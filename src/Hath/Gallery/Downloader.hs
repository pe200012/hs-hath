{-# LANGUAGE DataKinds #-}

-- | The gallery downloader: works through the galleries queued for this client
-- on the H\@H site, into the download directory, as the official
-- GalleryDownloader does.
module Hath.Gallery.Downloader ( runGalleryDownloader, downloadQueue, anotherRound ) where

import           Colog                   ( Message )
import           Colog.Polysemy          ( Log )

import           Control.Concurrent      ( threadDelay )

import qualified Data.ByteString.Char8   as BS
import           Data.String.Interpolate ( i )

import           Hath.AtomicFile         ( writeFileAtomic )
import           Hath.Config             ( ClientConfig(..) )
import           Hath.Download           ( fetchFile )
import           Hath.Error              ( HathError )
import           Hath.Gallery            ( GalleryFile(..)
                                         , GalleryMetadata(..)
                                         , fallbackDirectoryName
                                         , galleryDirectoryName
                                         )
import           Hath.Log                ( logInfo, logWarning )
import           Hath.Protocol           ( sha1Hex )
import           Hath.RPC                ( RPC
                                         , galleryFileSource
                                         , nextGallery
                                         , reportDownloadFailures
                                         , runRPCIO
                                         )
import           Hath.Settings           ( HathSettings(..) )
import           Hath.Stats              ( StatsEnv, addDlBytes, incDlFile, incDlTask )

import qualified Network.HTTP.Client     as HC

import           Polysemy
import           Polysemy.Error          ( Error, catch )
import           Polysemy.Reader         ( Reader, ask, runReader )

import           Relude                  hiding ( Reader, ask, runReader )

import           System.Directory        ( createDirectoryIfMissing, removeFile )
import           System.FilePath         ( (<.>), (</>) )
import           System.IO               ( hPutStrLn )

import           UnliftIO                ( tryAny )

-- | Work through the download queue each time the server asks
-- (@start_downloader@ fills @requested@). A request that comes while a run is
-- under way is dropped, as the official client ignores it
-- (HentaiAtHomeClient.startDownloader): the run goes on until the queue is
-- empty anyway.
runGalleryDownloader :: ClientConfig -> StatsEnv -> TVar HathSettings -> MVar () -> IO Void
runGalleryDownloader config stats settings requested = infinitely $ do
  takeMVar requested
  tryAny (downloadQueue config stats settings) >>= \case
    Left e           -> failed (show e)
    Right (Left e)   -> failed (show e)
    Right (Right ()) -> pass
  void $ tryTakeMVar requested
  where
    failed e = hPutStrLn stderr $ "Gallery downloader error: " <> e

-- | Download the queued galleries one after another (GalleryDownloader.java:58-126).
-- Each fetchqueue after the first marks the gallery before it done, whether or
-- not all of its files came, and brings the next one. The run ends when the
-- queue is empty, when the server refuses the request, or when we refuse its list.
downloadQueue :: ClientConfig -> StatsEnv -> TVar HathSettings -> IO (Either HathError ())
downloadQueue config stats settings = runRPCIO config $ runReader stats $ go Nothing
  where
    go done = nextGallery done >>= \case
      Left "NO_PENDING_DOWNLOADS" -> logInfo "No more galleries to download"
      Left reason -> logWarning [i|The gallery downloader stops: #{reason}|]
      Right gallery -> do
        incDlTask
        current <- embed @IO (readTVarIO settings)
        galleryDirectory (maxFilenameLength current) gallery >>= \case
          Nothing        -> logWarning
            [i|Unexpected download location for gallery #{galleryID gallery}; the gallery downloader stops|]
          Just directory -> do
            downloadGallery (maxAllowedFileSize current) directory gallery
            go (Just gallery)

-- | Make the gallery's directory and give its path. If the file system will
-- not take the name, the official client suspects it cannot store Unicode and
-- falls back on one without the title (GalleryDownloader.java:287-298).
-- Nothing if the name would leave the download directory.
galleryDirectory :: Members '[ Reader ClientConfig, Log Message, Embed IO ] r
                 => Int
                 -> GalleryMetadata
                 -> Sem r (Maybe FilePath)
galleryDirectory maxLength gallery = do
  downloads <- toString . downloadDir <$> ask
  forM (galleryDirectoryName maxLength gallery) $ \name -> do
    let path = downloads </> toString name
    embed (tryAny (createDirectoryIfMissing True path)) >>= \case
      Right () -> pure path
      Left e   -> do
        logWarning
          [i|Could not create the gallery directory "#{name}" (#{e}); using one without the title|]
        let fallback = downloads </> toString (fallbackDirectoryName gallery)
        embed $ createDirectoryIfMissing True fallback
        pure fallback

-- | Download a gallery's files, write its @galleryinfo.txt@ if all of them came
-- (GalleryDownloader.java:132-150), and report the sources that failed.
downloadGallery :: Members '[ RPC, Reader StatsEnv, Log Message, Error HathError, Embed IO ] r
                => Int
                -> FilePath
                -> GalleryMetadata
                -> Sem r ()
downloadGallery maxSize directory gallery = do
  logInfo [i|Starting download of gallery: #{galleryTitle gallery}|]
  ( complete, failures ) <- downloadFiles maxSize directory gallery
  if complete
    then do
      logInfo [i|Finished download of gallery: #{galleryTitle gallery}|]
      whenLeftM_
        (embed
         $ tryAny
         $ writeFileAtomic (directory </> "galleryinfo.txt") (galleryInformation gallery))
        $ \e -> logWarning [i|Could not write galleryinfo.txt: #{e}|]
    else logWarning [i|Permanently failed downloading gallery: #{galleryTitle gallery}|]
  -- the official client goes on whether or not the report gets through
  reportDownloadFailures failures `catch` \(e :: HathError)
    -> logWarning [i|Could not report download failures: #{e}|]

-- | Fetch the gallery's files into @directory@, going through those still
-- missing again while 'anotherRound' allows (GalleryDownloader.java:71-118).
-- True if all of them are there in the end; and the sources that failed, as
-- @host-fileindex-xres@.
downloadFiles :: Members '[ RPC, Reader StatsEnv, Log Message, Error HathError, Embed IO ] r
              => Int
              -> FilePath
              -> GalleryMetadata
              -> Sem r ( Bool, [ Text ] )
downloadFiles maxSize directory gallery = go 0 0 [] [ ( file, 1 ) | file <- files ]
  where
    files = galleryFileList gallery

    -- the files still missing, each with the number its next attempt gets
    go rounds failed failures missing
      | null missing = pure ( True, failures )
      | not (anotherRound (length files) rounds failed) = pure ( False, failures )
      | otherwise = do
        outcomes <- mapM (uncurry (ensureFile maxSize directory gallery)) missing
        let stillMissing
              = [ ( file, attempt + 1 ) | ( ( file, attempt ), Failed _ ) <- zip missing outcomes ]
        go
          (rounds + 1)
          (failed + length stillMissing)
          (failures <> [ source | Failed (Just source) <- outcomes ])
          stillMissing

-- | Whether to go through a gallery's missing files once more, after this many
-- rounds and failed downloads (GalleryDownloader.java:74): at most nine rounds,
-- and none once the failures reach twice the number of files.
anotherRound :: Int -> Int -> Int -> Bool
anotherRound fileCount rounds failed = rounds < 9 && failed < fileCount * 2

-- | How getting one file went.
data Outcome
  = Present
    -- ^ it was there already, intact
  | Downloaded
  | Failed !(Maybe Text)
-- ^ with the source that failed, if the server named one

-- | Get one file into the directory unless it is there already, intact
-- (GalleryDownloader.java:392-474): ask the server where to download it from,
-- this being attempt @attempt@ at it, and check what comes. The official
-- client then waits 1 s after a download, 5 s after a failure.
ensureFile :: Members '[ RPC, Reader StatsEnv, Log Message, Error HathError, Embed IO ] r
           => Int
           -> FilePath
           -> GalleryMetadata
           -> GalleryFile
           -> Int
           -> Sem r Outcome
ensureFile maxSize directory gallery file attempt = embed (intactAt path file) >>= \case
  True  -> pure Present
  False -> do
    outcome <- download
    embed $ threadDelay $ case outcome of
      Downloaded -> 1000000
      _          -> 5000000
    pure outcome
  where
    name = BS.unpack (galleryFileName file) <.> BS.unpack (galleryFileExt file)

    path = directory </> name

    download = do
      source <- galleryFileSource gallery file attempt `catch` \(e :: HathError)
        -> Nothing <$ logWarning [i|dlfetch failed for #{name}: #{e}|]
      case HC.parseRequest . BS.unpack =<< source of
        Nothing      -> do
          logWarning [i|No source to download #{name} from|]
          pure (Failed Nothing)
        Just request -> embed (fetchBytes maxSize request) >>= \case
          Right bytes
            | hasExpectedHash file bytes -> do
              embed $ writeFileAtomic path bytes
              incDlFile
              addDlBytes (BS.length bytes)
              logInfo [i|Downloaded #{name} of gallery #{galleryID gallery}|]
              pure Downloaded
          Right _     -> failedFrom request "its SHA-1 is not the one listed"
          Left why    -> failedFrom request why

    failedFrom request (why :: Text) = do
      logWarning [i|Failed to download #{name} (attempt #{attempt}): #{why}|]
      pure $ Failed $ Just [i|#{HC.host request}-#{galleryFileIndex file}-#{galleryFileXRes file}|]

-- | Whether the file is there, not empty, and has the expected hash if one is
-- known. One that is not is removed, as by the official client.
intactAt :: FilePath -> GalleryFile -> IO Bool
intactAt path file = do
  content <- fromRight "" <$> tryAny (readFileBS path)
  let intact = not (BS.null content) && hasExpectedHash file content
  unless intact $ void $ tryAny $ removeFile path
  pure intact

-- | A file generated on demand has no known hash; like the official client, accept it.
hasExpectedHash :: GalleryFile -> ByteString -> Bool
hasExpectedHash file content = maybe True (== sha1Hex content) (galleryFileHash file)

-- | Download a file into memory with 'fetchFile'.
fetchBytes :: Int -> HC.Request -> IO (Either Text ByteString)
fetchBytes maxSize request = do
  chunks <- newIORef []
  -- a chunk at offset 0 begins another attempt, so what came before is dropped
  let keep 0 chunk = writeIORef chunks [ chunk ]
      keep _ chunk = modifyIORef' chunks (chunk :)
  fetched <- fetchFile maxSize request keep
  forM fetched $ \_ -> BS.concat . reverse <$> readIORef chunks
