
{-# LANGUAGE CPP #-}

module Version ( versionString ) where

import           Relude

versionString :: String
versionString = __DATE__ <> " " <> __TIME__