{-# LANGUAGE DataKinds #-}

-- | What the web server runs with, and the effects its handlers run in.
module Hath.Server.App ( ServerEnv(..), ServerAction(..), App, runApp ) where

import           Colog                    ( Message, richMessageAction )
import           Colog.Polysemy           ( Log, runLogAction )

import           Hath.Cache               ( Cache, CacheRunner(..) )
import           Hath.Config              ( ClientConfig )
import           Hath.Error               ( HathError )
import           Hath.Gallery.Downloader  ( GalleryTask )
import           Hath.RPC                 ( RPC, runRPC )
import           Hath.Server.FloodControl ( FloodControl )
import           Hath.Settings            ( HathSettings )
import           Hath.Stats               ( StatsEnv )

import           Polysemy
import           Polysemy.Error           ( Error, errorToIOFinal, mapError )
import           Polysemy.Reader          ( Reader, runReader )

import           Relude                   hiding ( Reader, runReader )

import           Servant                  ( Handler(..), ServerError, err500 )

import           UnliftIO                 ( TChan )

-- | Commands to the server loop.
data ServerAction
  = Cert
    -- ^ fetch a new certificate and serve it from now on
  | GracefulShutdown
-- ^ tell the RPC server we are leaving, then exit

data ServerEnv
  = ServerEnv
  { envConfig :: !ClientConfig
  , envSettings :: !(TVar HathSettings)
    -- ^ as assigned by the RPC server, and refreshed on its command
  , envActions :: !(TChan ServerAction)
  , envGalleryTasks :: !(TChan GalleryTask)
  , envStats :: !StatsEnv
  , envCache :: !CacheRunner
  , envFloodControl :: !(Maybe FloodControl)
    -- ^ Nothing: flood control is off (@--disable-rate-limit@)
  , envTrustProxyHeaders :: !Bool
    -- ^ take the reader's address from @X-Forwarded-For@
  }

-- | The effects of a request handler.
type App
  = Sem
    '[ Cache
     , RPC
     , Log Message
     , Reader StatsEnv
     , Reader ClientConfig
     , Error HathError
     , Error ServerError
     , Embed IO
     , Final IO
     ]

-- | Run a handler. The effects are interpreted afresh for every request, so
-- whatever must outlive one request lives in the 'ServerEnv'. An error the
-- handler did not turn into a response is a 500.
runApp :: ServerEnv -> App a -> Handler a
runApp env
  = Handler
  . ExceptT
  . runFinal
  . embedToFinal @IO
  . errorToIOFinal @ServerError
  . mapError @HathError (const err500)
  . runReader (envConfig env)
  . runReader (envStats env)
  . runLogAction @IO @Message richMessageAction
  . runRPC
  . runCacheWith (envCache env)
