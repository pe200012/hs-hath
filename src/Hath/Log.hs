module Hath.Log ( logInfo, logWarning, logError ) where

import           Colog          ( Message, Msg(..), Severity(..) )
import           Colog.Polysemy ( Log )
import qualified Colog.Polysemy as Colog

import           GHC.Stack      ( emptyCallStack )

import           Polysemy       ( Member, Sem )

import           Relude

logInfo, logWarning, logError :: Member (Log Message) r => Text -> Sem r ()
logInfo = logAt Info

logWarning = logAt Warning

logError = logAt Error

logAt :: Member (Log Message) r => Severity -> Text -> Sem r ()
logAt severity text
  = Colog.log Msg { msgSeverity = severity, msgStack = emptyCallStack, msgText = text }
