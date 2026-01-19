{-# LANGUAGE TemplateHaskell #-}

module Version ( versionInfo ) where

import           Control.Exception   ( catch )

import qualified Data.Text           as T
import           Data.Time.Clock     ( getCurrentTime )
import           Data.Time.Format    ( defaultTimeLocale, formatTime )

import           Language.Haskell.TH ( runIO, stringE )

import           Relude

import           System.Process      ( readProcess )

-- | Version information string
versionInfo :: T.Text
versionInfo
  = T.unlines
    [ "hs-hath " <> commitStr <> " (" <> dateStr <> ")"
    , "Build Commit: " <> commitStr
    , "Build Date:   " <> dateStr
    ]
  where
    commitStr :: T.Text
    commitStr
      = T.strip $ T.pack $( runIO (do
                                     c <- catch
                                       (readProcess "git" [ "rev-parse", "--short=7", "HEAD" ] "")
                                       (\(_ :: SomeException) -> return "unknown")
                                     stringE c) )

    dateStr :: T.Text
    dateStr
      = T.strip $ T.pack $( runIO (do
                                     t <- getCurrentTime
                                     let d = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" t
                                     stringE d) )
