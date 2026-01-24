module Middleware
  ( rateLimitMiddleware
  , tracingConnections
  , normalizeAcceptMiddleware
  , tracingTimeUsage
  , IPMap
  , KeystampMap
  , IP(..)
  , IPRecord(..)
  , timeWindow
  ) where

import           Control.Exception   ( finally )

import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HashMap

import           GHC.Clock           ( getMonotonicTime )

import qualified Metrics.Gauge       as Gauge

import           Network.HTTP.Types  ( hAccept )
import           Network.Socket      ( HostAddress, HostAddress6, SockAddr(..) )
import           Network.Wai         ( Middleware, rawPathInfo, remoteHost, requestHeaders )

import           Relude              hiding ( Reader, ask, get, runReader )

import           Stats               ( StatsEnv(..) )

import           System.IO.Unsafe    ( unsafePerformIO )

import           Utils               ( tooManyRequestsResponse )

timeWindow :: Double
timeWindow = 10 * 60 -- 10 minutes in seconds

maxRequests :: Int8
maxRequests = 5

-- Data types for tracking requests
-- Note: Lists cannot be unpacked, so we use strict spine with explicit strictness
data IPRecord
  = IPRecord { earlistRequestTime :: !Double   -- ^ Time of the first request
             , requestTimes       :: !Int8     -- ^ Number of requests in the time window
             , bannedUntil        :: !Double -- ^ When the ban expires
             }
  deriving ( Show )

-- Change the Map key from SockAddr to a custom IP type
data IP = IPv4 {-# UNPACK #-} !HostAddress | IPv6 {-# UNPACK #-} !HostAddress6
  deriving ( Eq, Ord, Show, Generic )

instance Hashable IP

type IPMap = HashMap IP IPRecord

{-# INLINE getIP #-}
-- Helper function to extract just the IP from a SockAddr
getIP :: SockAddr -> Maybe IP
getIP (SockAddrInet _ ha) = Just $ IPv4 ha
getIP (SockAddrInet6 _ _ ha _) = Just $ IPv6 ha
getIP _ = Nothing

{-# NOINLINE globalIPMap #-}
globalIPMap :: IORef IPMap
globalIPMap = unsafePerformIO $ newIORef HashMap.empty

{-# NOINLINE globalIPMapCounter #-}
globalIPMapCounter :: IORef Int
globalIPMapCounter = unsafePerformIO $ newIORef 0

-- Create a new rate limiting middleware
rateLimitMiddleware :: Middleware
rateLimitMiddleware app req k
  | "/h/" `BS.isPrefixOf` rawPathInfo req = do
    now <- getMonotonicTime
    let sockAddr = remoteHost req
    case getIP sockAddr of
      Nothing -> app req k  -- If we can't get IP, just allow the request
      Just ip -> do
        allowed <- checkRateLimit ip now
        if allowed
          then app req k
          else k tooManyRequestsResponse
  | otherwise = app req k  -- Skip rate limiting for non-resource paths

-- Check if request is allowed and update rate limit state
--
-- return False if the request should be blocked
checkRateLimit :: IP -> Double -> IO Bool
checkRateLimit ip now = do
  cnt <- readIORef globalIPMapCounter
  if cnt > 100
    then do
      writeIORef globalIPMapCounter 0
      writeIORef globalIPMap HashMap.empty
      pure True
    else do
      writeIORef globalIPMapCounter (cnt + 1)
      m <- readIORef globalIPMap
      maybe firstRequest existingRequest (HashMap.lookup ip m)
  where
    -- Handle first request from an IP by creating new record
    firstRequest = do
      modifyIORef' globalIPMap $ HashMap.insert ip (IPRecord now 1 0)
      return True

    -- Check if IP is banned, otherwise check request count
    existingRequest record
      | bannedUntil record > now = return False
      | otherwise = checkAndUpdateRequests record

    -- Check if requests are within rate limit window and update accordingly
    checkAndUpdateRequests record
      = if now - earlistRequestTime record <= timeWindow
        then if requestTimes record > maxRequests
          then do
            -- Ban the IP for 5 minutes
            modifyIORef' globalIPMap $ HashMap.insert ip (record { bannedUntil = now + 5 * 60 })
            return False
          else do
            modifyIORef' globalIPMap
              $ HashMap.insert ip (record { requestTimes = requestTimes record + 1 })
            return True
        else return True

type KeystampMap = HashMap ByteString ( Int, Double )

-- | Track active connections
tracingConnections :: StatsEnv -> Middleware
tracingConnections statsEnv app req k = do
  Gauge.inc (statsActiveConnectionsGauge statsEnv)
  finally (app req k) (Gauge.dec (statsActiveConnectionsGauge statsEnv))

normalizeAcceptMiddleware :: Middleware
normalizeAcceptMiddleware app req = app req { requestHeaders = normalizedHeaders }
  where
    normalizedHeaders = map normalizeHeader (requestHeaders req)

    normalizeHeader ( name, value )
      | name == hAccept = ( name, "*/*" )
      | otherwise = ( name, value )

tracingTimeUsage :: StatsEnv -> Middleware
tracingTimeUsage statsEnv app req k = do
  startTime <- getMonotonicTime
  finally (app req k) (do
                         endTime <- getMonotonicTime
                         let latency = endTime - startTime
                         Gauge.set (round (latency * 1000000)) (statsLatency statsEnv))

