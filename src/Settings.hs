{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Settings ( Settings(..), getSettings, updateSettings, runSettings ) where

import           Polysemy
import           Polysemy.Operators

import           Relude             hiding ( Reader )

import           Types              ( HathSettings )

data Settings m a where
  -- | Get current H@H settings
  GetSettings :: Settings m HathSettings
  -- | Update H@H settings (hot update without restart)
  UpdateSettings :: HathSettings -> Settings m ()

makeSem ''Settings

{-# INLINE runSettings #-}
-- | Run the Settings effect with a TVar storing the current settings
runSettings :: Member (Embed IO) r => TVar HathSettings -> Settings : r @> a -> r @> a
runSettings tvar = interpret $ \case
  GetSettings        -> embed $ readTVarIO @IO tvar
  UpdateSettings new -> embed $ atomically @IO $ writeTVar tvar new
