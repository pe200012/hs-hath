
{-# LANGUAGE CPP #-}

module Version ( versionString ) where

import           Relude

versionString :: Text
versionString = __DATE__ <> " " <> __TIME__