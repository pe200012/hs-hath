-- | The cap on open connections, and the overload notice near it, as the
-- official client applies them to a connection it accepts
-- (HTTPServer.java:242-255).
module Hath.Server.ConnectionLimit ( Admission(..), admission, overloadDue, connectionLimit ) where

import           Control.Concurrent       ( forkIO )

import           Data.String.Interpolate  ( i )

import           GHC.Clock                ( getMonotonicTime )

import           Hath.Server.FloodControl ( isLocalIP, isRPCServer, sockAddrIP )
import           Hath.Settings            ( HathSettings(rpcServerIPs), maxConnections )

import           Network.Socket           ( SockAddr )

import           Relude

data Admission
  = Refused
  | Admitted
  | AdmittedNearLimit
    -- ^ more than 80 % of the limit is taken: time to tell the server
  deriving ( Eq, Show )

-- | Admit a connection under this limit while so many others are open: the
-- official client counts the connections open before this one, exempt ones
-- included.
admission :: Int -> Int -> Admission
admission limit open
  | open > limit = Refused
  | open * 5 > limit * 4 = AdmittedNearLimit
  | otherwise = Admitted

-- | Whether to send an overload notice at this monotonic time, given when the
-- last one went: at most one every 30 s (ServerHandler.notifyOverload).
overloadDue :: Double -> Maybe Double -> Bool
overloadDue now = maybe True (< now - 30)

-- | Warp's 'Network.Wai.Handler.Warp.setOnOpen' and
-- 'Network.Wai.Handler.Warp.setOnClose' hooks for the limit. A refused
-- connection is closed without a reply. Near the limit, the notice runs on a
-- thread of its own, so that the connection does not wait for the RPC server.
connectionLimit :: TVar HathSettings -> IO () -> IO ( SockAddr -> IO Bool, SockAddr -> IO () )
connectionLimit settingsVar notifyOverload = do
  open <- newIORef (0 :: Int)
  lastNotice <- newIORef Nothing
  let onOpen peer = do
        before <- atomicModifyIORef' open $ \n -> ( n + 1, n )
        settings <- readTVarIO settingsVar
        if exempt settings peer
          then pure True
          else case admission (maxConnections settings) before of
            Refused           -> False
              <$ putStrLn
                [i|Exceeded the maximum allowed number of incoming connections (#{maxConnections settings}).|]
            Admitted          -> pure True
            AdmittedNearLimit -> do
              now <- getMonotonicTime
              due <- atomicModifyIORef' lastNotice $ \sent -> if overloadDue now sent
                then ( Just now, True )
                else ( sent, False )
              when due $ void $ forkIO notifyOverload
              pure True
      -- Warp calls this for a refused connection as well
      onClose _ = atomicModifyIORef' open $ \n -> ( n - 1, () )
  pure ( onOpen, onClose )
  where
    -- local addresses and the RPC servers' are not limited, as in the official
    -- client; nor is a peer without an IP address (a Unix socket)
    exempt settings
      = maybe True (\ip -> isLocalIP ip || isRPCServer (rpcServerIPs settings) ip) . sockAddrIP
