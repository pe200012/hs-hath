{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module SettingM ( SettingM(..), getSettings, updateSettings, runSettingM ) where

import           Polysemy
import           Polysemy.Operators

import           Relude             hiding ( Reader )

import           Types              ( HathSettings )

data SettingM m a where
  -- | Get current H@H settings
  GetSettings :: SettingM m HathSettings
  -- | Update H@H settings (hot update without restart)
  UpdateSettings :: HathSettings -> SettingM m ()

makeSem ''SettingM

{-# INLINE runSettingM #-}
-- | Run the Settings effect with a TVar storing the current settings
runSettingM :: Member (Embed IO) r => TVar HathSettings -> SettingM : r @> a -> r @> a
runSettingM tvar = interpret $ \case
  GetSettings        -> embed $ readTVarIO @IO tvar
  UpdateSettings new -> embed $ atomically @IO $ writeTVar tvar new
