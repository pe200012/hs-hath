{-# LANGUAGE DataKinds #-}

-- | What the web server runs with, and the effects its handlers run in.
module Hath.Server.App ( ServerEnv(..), ServerAction(..), App, runApp ) where

import           Colog                    ( Message, richMessageAction )
import           Colog.Polysemy           ( Log, runLogAction )

import           Hath.Cache               ( Cache, CacheRunner(..) )
import           Hath.Config              ( ClientConfig )
import           Hath.Error               ( HathError )
import           Hath.RPC                 ( RPC, runRPC )
import           Hath.Server.FloodControl ( FloodControl )
import           Hath.Settings            ( HathSettings )
import           Hath.Stats               ( StatsEnv )

import           Network.HTTP.Types       ( Status, status500 )

import           Polysemy
import           Polysemy.Error           ( Error, errorToIOFinal, mapError )
import           Polysemy.Reader          ( Reader, runReader )

import           Relude                   hiding ( Reader, runReader )

import           UnliftIO                 ( TChan )

-- | Commands to the server loop.
data ServerAction
  = Cert
    -- ^ fetch a new certificate and serve it from now on
  | GracefulShutdown
-- ^ tell the RPC server we are leaving, let open transfers finish, then return

data ServerEnv
  = ServerEnv
  { envConfig :: !ClientConfig
  , envSettings :: !(TVar HathSettings)
    -- ^ as assigned by the RPC server, and refreshed on its command
  , envActions :: !(TChan ServerAction)
  , envDownloadRequest :: !(MVar ())
    -- ^ filled by @start_downloader@ for the gallery downloader
  , envStats :: !StatsEnv
  , envCache :: !CacheRunner
  , envFloodControl :: !(Maybe FloodControl)
    -- ^ Nothing: flood control is off (@--disable-rate-limit@)
  , envTrustProxyHeaders :: !Bool
    -- ^ take the reader's address from @X-Forwarded-For@
  }

-- | The effects of a request handler. It ends a request with an error by
-- throwing its status.
type App
  = Sem
    '[ Cache
     , RPC
     , Log Message
     , Reader StatsEnv
     , Reader ClientConfig
     , Error HathError
     , Error Status
     , Embed IO
     , Final IO
     ]

-- | Run a handler. The effects are interpreted afresh for every request, so
-- whatever must outlive one request lives in the 'ServerEnv'. Left is the error
-- status the handler threw, or 500 for an error it did not handle.
runApp :: ServerEnv -> App a -> IO (Either Status a)
runApp env
  = runFinal
  . embedToFinal @IO
  . errorToIOFinal @Status
  . mapError @HathError (const status500)
  . runReader (envConfig env)
  . runReader (envStats env)
  . runLogAction @IO @Message richMessageAction
  . runRPC
  . runCacheWith (envCache env)
