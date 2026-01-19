{-# LANGUAGE RecordWildCards #-}

module CLI ( Options(..), parseOptions, applyOptionsToConfig ) where

import           Options.Applicative

import           Relude

import           Types               ( ClientConfig(..) )

-- | Command line options
data Options
  = Options
  { optConfigPath         :: !FilePath      -- ^ Path to configuration file
  , optDownloadDir        :: !(Maybe Text)  -- ^ Override download directory
  , optCachePath          :: !(Maybe Text)  -- ^ Override cache path
  , optSkipStartupVerify  :: !Bool         -- ^ Skip startup cache verification
  , optSkipPeriodicVerify :: !Bool        -- ^ Skip periodic file verification
  , optDisableRateLimit   :: !Bool          -- ^ Disable rate limiting (for NAT/proxy scenarios)
  , optTrustProxyHeaders  :: !Bool         -- ^ Trust X-Forwarded-For header for real IP
  , optShowVersion        :: !Bool          -- ^ Show version information
  }
  deriving ( Show )

-- | Parse command line options
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts
      = info
        (optionsParser <**> helper)
        (fullDesc
         <> progDesc "Haskell implementation of the Hath client"
         <> header "hs-hath - A Hentai@Home client in Haskell")

-- | Options parser
optionsParser :: Parser Options
optionsParser
  = Options
  <$> strOption
    (long "config"
     <> short 'c'
     <> metavar "FILE"
     <> value "./client-login"
     <> help "Configuration file path (default: ./client-login)")
  <*> optional
    (strOption
       (long "download-dir"
        <> short 'd'
        <> metavar "DIR"
        <> help "Override download directory from config"))
  <*> optional
    (strOption (long "cache-path" <> metavar "PATH" <> help "Override cache path from config"))
  <*> switch (long "skip-startup-verify" <> help "Skip cache integrity verification at startup")
  <*> switch
    (long "disable-periodic-verification"
     <> help "Disable periodic file verification during runtime")
  <*> switch
    (long "disable-rate-limit"
     <> help "Disable rate limiting (use when behind NAT/proxy that doesn't forward real IP)")
  <*> switch
    (long "trust-proxy-headers"
     <> help "Trust X-Forwarded-For header for real client IP (use when behind trusted proxy)")
  <*> switch (long "version" <> short 'v' <> help "Show version information and exit")

-- | Apply CLI options to client configuration
applyOptionsToConfig :: Options -> ClientConfig -> ClientConfig
applyOptionsToConfig Options { .. } config
  = config { downloadDir = fromMaybe (downloadDir config) optDownloadDir
           , cachePath   = fromMaybe (cachePath config) optCachePath
           }
