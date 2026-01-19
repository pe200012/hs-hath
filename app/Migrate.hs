{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Migrate ( main ) where

import           Colog                  ( LogAction(..), Message, Severity(..), richMessageAction )
import           Colog.Polysemy         ( Log, runLogAction )

import qualified Conduit                as C

import           Control.Exception      ( SomeException, try )
import           Control.Monad          ( when )

import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import           Database.SQLite.Simple ( Connection, Only(..), Query, fold_, open, query_ )

import           Network.Minio          ( AccessKey(..)
                                        , CredentialValue(..)
                                        , SecretKey(..)
                                        , defaultPutObjectOptions
                                        , runMinio
                                        , setCreds
                                        , setRegion
                                        )
import qualified Network.Minio          as Minio

import           Options.Applicative

import           Polysemy
import           Polysemy.Error         ( Error, runError )
import           Polysemy.KVStore       ( updateKV )

import           Relude                 hiding ( Reader )

import           Storage.Database       ( FileRecord(..), initializeDB )
import           Storage.R2             ( R2Connection(..), mkR2Connection, runCacheR2 )

import           System.Directory       ( doesDirectoryExist, doesFileExist, listDirectory )
import           System.FilePath        ( (</>), takeFileName )

import           Types                  ( FileURI, R2Config(..), RPCError(..), parseFileURI )

-- | Command line options
data MigrateOptions
  = MigrateOptions
  { optSourceType :: !SourceType
  , optSourcePath :: !FilePath
  , optR2Endpoint :: !Text
  , optR2Bucket   :: !Text
  , optDryRun     :: !Bool
  , optBatchSize  :: !Int
  , optVerbose    :: !Bool
  }
  deriving ( Show )

data SourceType = SQLiteSource | FilesystemSource
  deriving ( Show, Eq )

-- | Parse command line options
parseOptions :: Parser MigrateOptions
parseOptions
  = MigrateOptions
  <$> option
    parseSourceType
    (long "source-type" <> short 't' <> metavar "TYPE" <> help "Source type: sqlite or filesystem")
  <*> strOption
    (long "source"
     <> short 's'
     <> metavar "PATH"
     <> help "Source path (SQLite db file or cache directory)")
  <*> strOption
    (long "endpoint"
     <> short 'e'
     <> metavar "URL"
     <> help "R2 endpoint URL (e.g., https://xxx.r2.cloudflarestorage.com)")
  <*> strOption (long "bucket" <> short 'b' <> metavar "NAME" <> help "R2 bucket name")
  <*> switch
    (long "dry-run" <> short 'n' <> help "Show what would be migrated without actually uploading")
  <*> option
    auto
    (long "batch-size"
     <> metavar "N"
     <> value 100
     <> showDefault
     <> help "Number of files to process per batch")
  <*> switch (long "verbose" <> short 'v' <> help "Enable verbose output")

parseSourceType :: ReadM SourceType
parseSourceType = eitherReader $ \case
  "sqlite"     -> Right SQLiteSource
  "filesystem" -> Right FilesystemSource
  "fs"         -> Right FilesystemSource
  other        -> Left $ "Unknown source type: " <> other <> ". Use 'sqlite' or 'filesystem'"

optsInfo :: ParserInfo MigrateOptions
optsInfo
  = info
    (parseOptions <**> helper)
    (fullDesc
     <> progDesc "Migrate cache files to Cloudflare R2"
     <> header "migrate-to-r2 - Cache migration tool for hs-hath")

main :: IO ()
main = do
  opts <- execParser optsInfo

  -- Check environment variables
  accessKeyMay <- lookupEnv "R2_ACCESS_KEY"
  secretKeyMay <- lookupEnv "R2_SECRET_KEY"

  when (isNothing accessKeyMay) $ do
    TIO.putStrLn "Error: R2_ACCESS_KEY environment variable not set"
    exitFailure

  when (isNothing secretKeyMay) $ do
    TIO.putStrLn "Error: R2_SECRET_KEY environment variable not set"
    exitFailure

  -- Create R2 connection
  let r2Cfg = R2Config { r2Endpoint = optR2Endpoint opts, r2Bucket = optR2Bucket opts }

  r2Result <- mkR2Connection r2Cfg
  case r2Result of
    Left err     -> do
      TIO.putStrLn $ "Error creating R2 connection: " <> err
      exitFailure
    Right r2Conn -> do
      TIO.putStrLn $ "Connected to R2: " <> optR2Endpoint opts <> "/" <> optR2Bucket opts

      case optSourceType opts of
        SQLiteSource     -> migrateSQLite opts r2Conn
        FilesystemSource -> migrateFilesystem opts r2Conn

-- | Migrate from SQLite database
migrateSQLite :: MigrateOptions -> R2Connection -> IO ()
migrateSQLite opts r2Conn = do
  TIO.putStrLn $ "Migrating from SQLite: " <> T.pack (optSourcePath opts)

  conn <- open (optSourcePath opts)

  -- Count total files
  [ Only totalCount ] <- query_ conn "SELECT COUNT(*) FROM files" :: IO [ Only Int ]
  TIO.putStrLn $ "Total files to migrate: " <> show totalCount

  when (optDryRun opts) $ do
    TIO.putStrLn "[DRY RUN] Would upload the following files:"

  -- Stream files from SQLite and upload to R2
  migratedCount <- newIORef (0 :: Int)
  errorCount <- newIORef (0 :: Int)

  void $ fold_ conn selectAllQuery ( (), 0 :: Int ) $ \( (), _idx ) record -> do
    let fileId = fileRecordFileId record
        uri    = parseFileURI (encodeUtf8 fileId)

    if optDryRun opts
      then do
        when (optVerbose opts) $ TIO.putStrLn $ "  Would upload: " <> fileId
      else do
        result <- uploadToR2 r2Conn uri record
        case result of
          Left err -> do
            atomicModifyIORef' errorCount (\c -> ( c + 1, () ))
            when (optVerbose opts)
              $ TIO.putStrLn
              $ "  Error uploading " <> fileId <> ": " <> show err
          Right () -> do
            atomicModifyIORef' migratedCount (\c -> ( c + 1, () ))
            when (optVerbose opts) $ TIO.putStrLn $ "  Uploaded: " <> fileId

    -- Progress update every batch
    let idx' = _idx + 1
    when (idx' `mod` optBatchSize opts == 0)
      $ TIO.putStrLn
      $ "  Progress: " <> show idx' <> "/" <> show totalCount

    pure ( (), idx' )

  finalMigrated <- readIORef migratedCount
  finalErrors <- readIORef errorCount

  TIO.putStrLn $ "\nMigration complete:"
  TIO.putStrLn $ "  Migrated: " <> show finalMigrated
  TIO.putStrLn $ "  Errors: " <> show finalErrors
  where
    selectAllQuery :: Query
    selectAllQuery = "SELECT lru_counter, s4, file_id, file_name, bytes FROM files"

-- | Migrate from filesystem cache
migrateFilesystem :: MigrateOptions -> R2Connection -> IO ()
migrateFilesystem opts r2Conn = do
  TIO.putStrLn $ "Migrating from filesystem: " <> T.pack (optSourcePath opts)

  exists <- doesDirectoryExist (optSourcePath opts)
  unless exists $ do
    TIO.putStrLn $ "Error: Directory does not exist: " <> T.pack (optSourcePath opts)
    exitFailure

  -- Filesystem cache structure: optSourcePath/s4/file_id
  -- Where s4 is first 4 chars of hash
  s4Dirs <- listDirectory (optSourcePath opts)

  when (optDryRun opts) $ TIO.putStrLn "[DRY RUN] Would upload the following files:"

  migratedCount <- newIORef (0 :: Int)
  errorCount <- newIORef (0 :: Int)

  forM_ s4Dirs $ \s4Dir -> do
    let s4Path = optSourcePath opts </> s4Dir
    isDir <- doesDirectoryExist s4Path
    when isDir $ do
      files <- listDirectory s4Path
      forM_ files $ \fileName -> do
        let filePath = s4Path </> fileName
            fileId   = T.pack fileName
            uri      = parseFileURI (encodeUtf8 fileId)

        isFile <- doesFileExist filePath
        when isFile $ do
          if optDryRun opts
            then do
              when (optVerbose opts) $ TIO.putStrLn $ "  Would upload: " <> fileId
            else do
              let bucket = r2ConnBucket r2Conn
                  key    = fileURIToKey uri
              result <- try @SomeException
                $ Minio.runMinioWith (r2MinioConn r2Conn)
                $ Minio.fPutObject bucket key filePath defaultPutObjectOptions
              case result of
                Left err          -> do
                  atomicModifyIORef' errorCount (\c -> ( c + 1, () ))
                  when (optVerbose opts)
                    $ TIO.putStrLn
                    $ "  Error uploading " <> fileId <> ": " <> show err
                Right (Left mErr) -> do
                  atomicModifyIORef' errorCount (\c -> ( c + 1, () ))
                  when (optVerbose opts)
                    $ TIO.putStrLn
                    $ "  Error uploading " <> fileId <> ": " <> show mErr
                Right (Right ())  -> do
                  atomicModifyIORef' migratedCount (\c -> ( c + 1, () ))
                  when (optVerbose opts) $ TIO.putStrLn $ "  Uploaded: " <> fileId

  finalMigrated <- readIORef migratedCount
  finalErrors <- readIORef errorCount

  TIO.putStrLn $ "\nMigration complete:"
  TIO.putStrLn $ "  Migrated: " <> show finalMigrated
  TIO.putStrLn $ "  Errors: " <> show finalErrors

-- | Upload a file record to R2
uploadToR2 :: R2Connection -> FileURI -> FileRecord -> IO (Either Text ())
uploadToR2 conn uri record = do
  let key     = fileURIToKey uri
      bucket  = r2ConnBucket conn
      content = fileRecordBytes record
      size    = fromIntegral (BS.length content) :: Int64
      src     = C.yield content

  result <- try @SomeException
    $ Minio.runMinioWith (r2MinioConn conn)
    $ Minio.putObject bucket key src (Just size) defaultPutObjectOptions

  case result of
    Left err          -> pure $ Left $ T.pack $ show err
    Right (Left mErr) -> pure $ Left $ T.pack $ show mErr
    Right (Right _)   -> pure $ Right ()

-- | Generate R2 object key from FileURI (same as in R2.hs)
fileURIToKey :: FileURI -> Text
fileURIToKey uri = s4 <> "/" <> fileId
  where
    fileId = show uri

    s4     = T.take 4 fileId
