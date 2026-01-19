{-# LANGUAGE TemplateHaskell #-}

module Version ( versionInfo ) where

import           Language.Haskell.TH       ( Q, Exp, stringE, runIO )
import qualified Data.Text                as T
import           System.Process           ( readProcess )
import           Data.Time.Clock          ( getCurrentTime )
import           Data.Time.Format         ( formatTime, defaultTimeLocale )
import           Control.Exception        ( catch, SomeException )
import           Relude                 hiding ( string )

-- | Version information string
versionInfo :: T.Text
versionInfo = T.unlines
  [ "hs-hath " <> commitStr <> " (" <> dateStr <> ")"
  , "Build Commit: " <> commitStr
  , "Build Date:   " <> dateStr
  ]
  where
    commitStr :: T.Text
    commitStr = T.strip $ T.pack $(runIO (do
      c <- catch (readProcess "git" ["rev-parse", "--short=7", "HEAD"] "")
                 (\(_ :: SomeException) -> return "unknown")
      stringE c))

    dateStr :: T.Text
    dateStr = T.strip $ T.pack $(runIO (do
      t <- getCurrentTime
      let d = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" t
      stringE d))
