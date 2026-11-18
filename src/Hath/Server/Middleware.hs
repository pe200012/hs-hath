module Hath.Server.Middleware ( rateLimitMiddleware, tracingConnections, tracingTimeUsage ) where

import           Control.Exception        ( finally )

import           GHC.Clock                ( getMonotonicTime )

import           Hath.Server.FloodControl ( FloodControl
                                          , IP
                                          , checkRateLimit
                                          , isLocalIP
                                          , sockAddrIP
                                          )
import           Hath.Stats               ( StatsEnv, observeDuration, trackConnection )

import           Network.HTTP.Types       ( mkStatus )
import           Network.Wai              ( Middleware, Response, remoteHost, responseLBS )

import           Relude

-- | Flood control, for a request to any path: the official client counts
-- every connection. Local addresses and those the predicate exempts (the RPC
-- servers, as in the official client) are never limited.
rateLimitMiddleware :: FloodControl -> (IP -> IO Bool) -> Middleware
rateLimitMiddleware floodControl exempt app req respond = case sockAddrIP (remoteHost req) of
  -- a peer without an IP address (a Unix socket) is not limited
  Nothing -> app req respond
  Just ip
    | isLocalIP ip -> app req respond
    | otherwise -> do
      now <- getMonotonicTime
      allowed <- ifM (exempt ip) (pure True) (checkRateLimit floodControl ip now)
      if allowed
        then app req respond
        else respond tooManyRequests

tooManyRequests :: Response
tooManyRequests
  = responseLBS
    (mkStatus 429 "Too Many Requests")
    [ ( "Content-Type", "text/plain" ) ]
    "Too Many Requests"

tracingConnections :: StatsEnv -> Middleware
tracingConnections stats app req respond = trackConnection stats (app req respond)

-- | Record how long each request took, the body included.
tracingTimeUsage :: StatsEnv -> Middleware
tracingTimeUsage stats app req respond = do
  start <- getMonotonicTime
  app req respond `finally` do
    end <- getMonotonicTime
    observeDuration stats (end - start)
