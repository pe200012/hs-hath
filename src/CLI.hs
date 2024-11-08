module CLI ( Options(..), getOptions ) where

import           Options.Applicative

import           Relude

-- | Command line options for the Hentai@Home client
data Options
    = Options { -- | Directory where downloaded gallery files will be stored
                optDownloadDir :: Maybe FilePath
                -- | Path to the SQLite database used for caching
              , optCachePath   :: Maybe FilePath
              }
    deriving ( Show )

-- | Parser for command line options
parseOptions :: Parser Options
parseOptions
    = Options
    <$> optional
        (strOption
             (long "download-dir" <> metavar "DIR" <> help "Directory to store downloaded files"))
    <*> optional
        (strOption
             (long "cache-path" <> metavar "PATH" <> help "Path to the SQLite cache database"))

-- | Parser info with descriptions and help text
opts :: ParserInfo Options
opts
    = info
        (parseOptions <**> helper)
        (fullDesc
         <> progDesc "Hentai@Home client"
         <> header "hath - A Hentai@Home client implementation")

-- | Get command line options by parsing arguments
getOptions :: IO Options
getOptions = execParser opts
