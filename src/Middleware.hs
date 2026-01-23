module Middleware
  ( rateLimitMiddleware
  , keystampLimitMiddleware
  , tracingConnections
  , normalizeAcceptMiddleware
  , IPMap
  , KeystampMap
  , IP(..)
  , IPRecord(..)
  , timeWindow
  ) where

import           Control.Exception     ( finally )

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Map.Strict       as Map
import           Data.Time.Clock       ( NominalDiffTime, UTCTime, addUTCTime, getCurrentTime )

import           GHC.Clock             ( getMonotonicTime )

import qualified Metrics.Gauge         as Gauge

import           Network.HTTP.Types    ( hAccept, mkStatus )
import           Network.Socket        ( HostAddress, HostAddress6, SockAddr(..) )
import           Network.Wai           ( Middleware
                                       , Response
                                       , rawPathInfo
                                       , remoteHost
                                       , requestHeaders
                                       , responseLBS
                                       )

import           Relude                hiding ( Reader, ask, get, runReader )

import           Stats                 ( StatsEnv(..) )

import           System.IO.Unsafe      ( unsafePerformIO )

import           Utils                 ( parseURLParams )

timeWindow :: Double
timeWindow = 10 * 60 -- 10 minutes in seconds

maxRequests :: Int
maxRequests = 5

-- Data types for tracking requests
-- Note: Lists cannot be unpacked, so we use strict spine with explicit strictness
data IPRecord
  = IPRecord { requestTimes :: ![ Double ]       -- ^ Times of recent requests (strict spine)
             , bannedUntil  :: !(Maybe Double) -- ^ When the ban expires
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

-- Create a new rate limiting middleware
rateLimitMiddleware :: TVar IPMap -> Middleware
rateLimitMiddleware ipMap app req k
  | "/h/" `BS.isPrefixOf` rawPathInfo req = do
    now <- getMonotonicTime
    let sockAddr = remoteHost req
    case getIP sockAddr of
      Nothing -> app req k  -- If we can't get IP, just allow the request
      Just ip -> do
        allowed <- checkRateLimit ipMap ip now
        if allowed
          then app req k
          else k tooManyRequestsResponse
  | otherwise = app req k  -- Skip rate limiting for non-resource paths

-- Check if request is allowed and update rate limit state
checkRateLimit :: TVar IPMap -> IP -> Double -> IO Bool
checkRateLimit ipMap ip now = atomically $ do
  m <- readTVar ipMap
  maybe firstRequest existingRequest (HashMap.lookup ip m)
  where
    -- Handle first request from an IP by creating new record
    firstRequest = do
      modifyTVar' ipMap $ HashMap.insert ip (IPRecord [ now ] Nothing)
      return True

    -- Check if IP is banned, otherwise check request count
    existingRequest record = case bannedUntil record of
      Just banTime
        | banTime > now -> return False
      _ -> checkAndUpdateRequests record

    -- Check if requests are within rate limit window and update accordingly
    checkAndUpdateRequests record = do
      let windowStart = now - timeWindow
          -- Use filter and force strict evaluation to avoid space leaks
          -- The list spine and length are evaluated immediately, preventing thunk accumulation
          !recentRequests = filter (> windowStart) (requestTimes record)
          !len = length recentRequests
          !newRequests = now : recentRequests

      if len >= maxRequests
        then banIP newRequests
        else allowRequest newRequests

    -- Ban IP for 10 minutes if too many requests
    banIP newRequests = do
      let banUntil = now + (10 * 60)  -- 10 minute ban
      modifyTVar' ipMap $ HashMap.insert ip (IPRecord newRequests (Just banUntil))
      return False

    -- Allow request and update record with new request time
    allowRequest newRequests = do
      modifyTVar' ipMap $ HashMap.insert ip (IPRecord newRequests Nothing)
      return True

{-# INLINE tooManyRequestsResponse #-}
-- Standard 429 response
tooManyRequestsResponse :: Response
tooManyRequestsResponse
  = responseLBS
    (mkStatus 429 "Too Many Requests")
    [ ( "Content-Type", "text/plain" ) ]
    "Too Many Requests"

type KeystampMap = HashMap ByteString ( Int, UTCTime )

-- | Keystamp rate limiting middleware
keystampLimitMiddleware :: TVar KeystampMap -> Middleware
keystampLimitMiddleware ksVar app req sendResponse
  | "/h/" `BS.isPrefixOf` path = do
    let parts = BS.split 47 (BS.drop 1 path) -- 47 is '/'
    case parts of
      (_h : fileid : optsRaw : _) -> do
        let opts = parseURLParams optsRaw
        case Map.lookup "keystamp" opts of
          Just ksRaw -> do
            let ( timestamp, _ ) = BSC.span (/= '-') ksRaw
                checkKey         = fileid <> timestamp
            now <- getCurrentTime
            allowed <- atomically $ do
              m <- readTVar ksVar
              let ( count, _ ) = HashMap.lookupDefault ( 0, now ) checkKey m
              if count >= 10  -- Undocumented limit
                then return False
                else do
                  let !newM = HashMap.insert checkKey ( count + 1, now ) m
                  writeTVar ksVar newM
                  return True
            if allowed
              then app req sendResponse
              else sendResponse tooManyRequestsResponse
          Nothing    -> app req sendResponse
      _ -> app req sendResponse
  | otherwise = app req sendResponse
  where
    path = rawPathInfo req

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

