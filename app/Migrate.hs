-- | migrate-to-r2: upload an SQLite or Filesystem cache to an R2 bucket.
module Migrate ( main ) where

import           Database.SQLite.Simple ( Only(..), fold_, query_, withConnection )

import           Hath.Cache.R2          ( R2Connection(..)
                                        , fileURIToKey
                                        , mkR2Connection
                                        , putR2Object
                                        , runR2
                                        )
import           Hath.Config            ( R2Config(..) )
import           Hath.FileURI           ( FileURI, parseFileURI )

import           Network.Minio          ( defaultPutObjectOptions )
import qualified Network.Minio          as Minio

import           Options.Applicative

import           Relude

import           System.Directory       ( doesDirectoryExist, doesFileExist, listDirectory )
import           System.FilePath        ( (</>) )

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

data SourceType = SQLiteSource | FilesystemSource

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
  mkR2Connection R2Config { r2Endpoint = optR2Endpoint opts, r2Bucket = optR2Bucket opts } >>= \case
    Left err -> do
      putTextLn $ "Error creating R2 connection: " <> err
      exitFailure
    Right r2 -> do
      putTextLn $ "Connected to R2: " <> optR2Endpoint opts <> "/" <> optR2Bucket opts
      tally <- case optSourceType opts of
        SQLiteSource     -> migrateSQLite opts r2
        FilesystemSource -> migrateFilesystem opts r2
      putTextLn "\nMigration complete:"
      putTextLn $ "  Migrated: " <> show (migrated tally)
      putTextLn $ "  Errors: " <> show (failed tally)

data Tally = Tally { migrated :: !Int, failed :: !Int }

migrateOne :: MigrateOptions -> Text -> (FileURI -> IO (Either Text ())) -> Tally -> IO Tally
migrateOne opts fileId upload tally
  | optDryRun opts = tally <$ say ("  Would upload: " <> fileId)
  | otherwise
    = maybe (pure $ Left "not a file id") upload (parseFileURI (encodeUtf8 fileId)) >>= \case
      Left err -> tally { failed = failed tally + 1 }
        <$ say ("  Error uploading " <> fileId <> ": " <> err)
      Right () -> tally { migrated = migrated tally + 1 } <$ say ("  Uploaded: " <> fileId)
  where
    say = when (optVerbose opts) . putTextLn

-- | Rows are streamed, so the database is never in memory as a whole.
migrateSQLite :: MigrateOptions -> R2Connection -> IO Tally
migrateSQLite opts r2 = withConnection (optSourcePath opts) $ \conn -> do
  putTextLn $ "Migrating from SQLite: " <> toText (optSourcePath opts)
  total <- maybe (0 :: Int) fromOnly . listToMaybe <$> query_ conn "SELECT COUNT(*) FROM files"
  putTextLn $ "Total files to migrate: " <> show total
  when (optDryRun opts) $ putTextLn "[DRY RUN] Would upload the following files:"
  fmap fst
    $ fold_ conn "SELECT file_id, bytes FROM files" ( Tally 0 0, 0 :: Int )
    $ \( tally, done ) ( fileId, bytes ) -> do
      tally' <- migrateOne opts fileId (\uri -> putR2Object r2 uri bytes) tally
      when ((done + 1) `mod` optBatchSize opts == 0)
        $ putTextLn
        $ "  Progress: " <> show (done + 1) <> "/" <> show total
      pure ( tally', done + 1 )

-- | The cache is @root\/xxxx\/\<file id>@, xxxx being the first four characters of the id.
migrateFilesystem :: MigrateOptions -> R2Connection -> IO Tally
migrateFilesystem opts r2 = do
  putTextLn $ "Migrating from filesystem: " <> toText root
  unlessM (doesDirectoryExist root) $ do
    putTextLn $ "Error: Directory does not exist: " <> toText root
    exitFailure
  when (optDryRun opts) $ putTextLn "[DRY RUN] Would upload the following files:"
  dirs <- filterM (doesDirectoryExist . (root </>)) =<< listDirectory root
  foldlM migrateDirectory (Tally 0 0) dirs
  where
    root = optSourcePath opts

    migrateDirectory tally dir = foldlM (migrateFile dir) tally =<< listDirectory (root </> dir)

    migrateFile dir tally name = do
      let path       = root </> dir </> name
          upload uri
            = runR2 r2
            $ Minio.fPutObject (r2ConnBucket r2) (fileURIToKey uri) path defaultPutObjectOptions
      ifM (doesFileExist path) (migrateOne opts (toText name) upload tally) (pure tally)
