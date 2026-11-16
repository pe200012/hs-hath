{-# LANGUAGE RecordWildCards #-}

module Hath.CLI ( Options(..), parseOptions, applyOptionsToConfig ) where

import           Hath.Config         ( ClientConfig(..) )
import           Hath.Version        ( versionInfo )

import           Options.Applicative

import           Relude

-- | Command line options
data Options
  = Options
  { optConfigPath        :: !FilePath      -- ^ Path to configuration file
  , optDownloadDir       :: !(Maybe Text)  -- ^ Override download directory
  , optCachePath         :: !(Maybe Text)  -- ^ Override cache path
  , optSkipStartupVerify :: !Bool         -- ^ Skip startup cache verification
  , optDisableRateLimit  :: !Bool          -- ^ Disable rate limiting (for NAT/proxy scenarios)
  , optTrustProxyHeaders :: !Bool         -- ^ Trust X-Forwarded-For header for real IP
  , optValidateConfig    :: !Bool          -- ^ Validate configuration and exit
  }
  deriving ( Show )

-- | Parse command line options
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts
      = info
        (optionsParser <**> versionOption <**> helper)
        (fullDesc
         <> progDesc "Haskell implementation of the Hath client"
         <> header "hs-hath - A Hentai@Home client in Haskell")

    versionOption
      = infoOption
        (toString versionInfo)
        (long "version" <> short 'v' <> help "Show version information and exit")

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
    (long "disable-rate-limit"
     <> help "Disable rate limiting (use when behind NAT/proxy that doesn't forward real IP)")
  <*> switch
    (long "trust-proxy-headers"
     <> help "Trust X-Forwarded-For header for real client IP (use when behind trusted proxy)")
  <*> switch (long "validate-config" <> help "Validate configuration and exit")

-- | Apply CLI options to client configuration
applyOptionsToConfig :: Options -> ClientConfig -> ClientConfig
applyOptionsToConfig Options { .. } config
  = config { downloadDir = fromMaybe (downloadDir config) optDownloadDir
           , cachePath   = fromMaybe (cachePath config) optCachePath
           }
