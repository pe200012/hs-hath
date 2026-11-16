{-# LANGUAGE TemplateHaskell #-}

-- | What this binary was built from, fixed when this module is compiled.
module Hath.Version ( versionInfo ) where

import           Control.Exception   ( catch )

import qualified Data.Text           as T
import           Data.Time.Clock     ( getCurrentTime )
import           Data.Time.Format    ( defaultTimeLocale, formatTime )

import           Language.Haskell.TH ( runIO, stringE )

import           Relude

import           System.Process      ( readProcess )

versionInfo :: Text
versionInfo
  = unlines
    [ "hs-hath " <> commit <> " (" <> date <> ")"
    , "Build Commit: " <> commit
    , "Build Date:   " <> date
    ]
  where
    -- "unknown" when the build does not run in a git checkout
    commit :: Text
    commit
      = T.strip
        $( runIO
          (readProcess "git" [ "rev-parse", "--short=7", "HEAD" ] "" `catch` \(_ :: SomeException)
           -> pure "unknown")
        >>= stringE
        )

    date :: Text
    date
      = $( runIO (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" <$> getCurrentTime)
        >>= stringE
        )
