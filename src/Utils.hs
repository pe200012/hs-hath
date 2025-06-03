
module Utils ( log ) where

import           Colog              ( Message, Msg(..), Severity )
import           Colog.Polysemy     ( Log )
import qualified Colog.Polysemy     as Co

import           Polysemy.Operators

import           Relude

-- msg :: sev -> Text -> Msg sev
-- msg sev m = withFrozenCallStack (Msg { msgSeverity = sev, msgStack = callStack, msgText = m })
{-# INLINE log #-}
log :: Severity -> Text -> Log Message -@> ()
log sev msg
    = withFrozenCallStack (Co.log Msg { msgSeverity = sev, msgStack = callStack, msgText = msg })
