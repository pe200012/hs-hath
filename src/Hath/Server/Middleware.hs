-- | What every request passes through before it reaches a handler.
module Hath.Server.Middleware
  ( rateLimitMiddleware
  , tracingConnections
  , tracingTimeUsage
  , normalizeAcceptMiddleware
  , withHathHeaders
  ) where

import           Control.Exception        ( finally )

import qualified Data.ByteString          as BS

import           GHC.Clock                ( getMonotonicTime )

import           Hath.Protocol            ( hathHeaders )
import           Hath.Server.FloodControl ( FloodControl
                                          , IP
                                          , checkRateLimit
                                          , isLocalIP
                                          , sockAddrIP
                                          )
import           Hath.Stats               ( StatsEnv, observeDuration, trackConnection )

import           Network.HTTP.Types       ( hAccept, mkStatus )
import           Network.Wai              ( Middleware
                                          , Response
                                          , mapResponseHeaders
                                          , rawPathInfo
                                          , remoteHost
                                          , requestHeaders
                                          , responseLBS
                                          )

import           Relude

-- | Flood control for @\/h\/@. Local addresses and those the predicate exempts
-- (the RPC servers, as in the official client) are never limited.
rateLimitMiddleware :: FloodControl -> (IP -> IO Bool) -> Middleware
rateLimitMiddleware floodControl exempt app req respond
  | "/h/" `BS.isPrefixOf` rawPathInfo req = case sockAddrIP (remoteHost req) of
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
  | otherwise = app req respond

tooManyRequests :: Response
tooManyRequests
  = responseLBS
    (mkStatus 429 "Too Many Requests")
    [ ( "Content-Type", "text/plain" ) ]
    "Too Many Requests"

-- | Count the requests being served.
tracingConnections :: StatsEnv -> Middleware
tracingConnections stats app req respond = trackConnection stats (app req respond)

-- | Record how long each request took, the body included.
tracingTimeUsage :: StatsEnv -> Middleware
tracingTimeUsage stats app req respond = do
  start <- getMonotonicTime
  app req respond `finally` do
    end <- getMonotonicTime
    observeDuration stats (end - start)

-- | Treat every request as accepting anything, so content negotiation never
-- turns a reader away.
normalizeAcceptMiddleware :: Middleware
normalizeAcceptMiddleware app req
  = app req { requestHeaders = map acceptAnything (requestHeaders req) }
  where
    acceptAnything ( name, value )
      | name == hAccept = ( name, "*/*" )
      | otherwise = ( name, value )

-- | Add 'hathHeaders' to a response, except those it already sets.
withHathHeaders :: Middleware
withHathHeaders app req respond = app req (respond . mapResponseHeaders addMissing)
  where
    addMissing headers = filter ((`notElem` map fst headers) . fst) hathHeaders ++ headers
