module Middleware
  ( rateLimitMiddleware
  , tracingConnections
  , normalizeAcceptMiddleware
  , tracingTimeUsage
  , withHentaiHeaders
  , IPMap
  , KeystampMap
  , IP(..)
  , IPRecord(..)
  , timeWindow
  , checkRateLimit
  , isLocalIP
  , parseIPv4
  ) where

import           Control.Exception   ( finally )

import           Data.Bits           ( shiftR, (.&.) )

import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HashMap

import           GHC.Clock           ( getMonotonicTime )

import qualified Metrics.Gauge       as Gauge
import qualified Metrics.Histogram   as Histogram

import           Network.HTTP.Types  ( hAccept )
import           Network.Socket      ( HostAddress
                                     , HostAddress6
                                     , SockAddr(..)
                                     , hostAddress6ToTuple
                                     , hostAddressToTuple
                                     , tupleToHostAddress
                                     )
import           Network.Wai         ( Middleware
                                     , mapResponseHeaders
                                     , rawPathInfo
                                     , remoteHost
                                     , requestHeaders
                                     )

import           Relude              hiding ( Reader, ask, get, runReader )

import           Stats               ( StatsEnv(..) )

import           System.IO.Unsafe    ( unsafePerformIO )

import           Types               ( hentaiHeader )

import           Utils               ( tooManyRequestsResponse )

-- | Mirrors the official client's flood control (HTTPServer.FloodControlEntry):
-- every request adds one hit, hits drain at one per second, and more than
-- 'floodLimit' hits blocks the IP for 'blockSeconds'.
floodLimit :: Int
floodLimit = 10

blockSeconds :: Double
blockSeconds = 60

-- | Entries idle for this long carry no state worth keeping (any block has expired).
timeWindow :: Double
timeWindow = 60

data IPRecord
  = IPRecord { hitCount    :: !Int     -- ^ Hits left after draining
             , lastHit     :: !Double  -- ^ Monotonic time of the last counted hit
             , bannedUntil :: !Double  -- ^ When the block expires
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

-- | Flood control for /h/. Local addresses and those the predicate exempts
-- (the RPC servers, as in the official client) are never limited.
rateLimitMiddleware :: (IP -> IO Bool) -> Middleware
rateLimitMiddleware exempt app req k
  | "/h/" `BS.isPrefixOf` rawPathInfo req = do
    now <- getMonotonicTime
    let sockAddr = remoteHost req
    case getIP sockAddr of
      Nothing -> app req k  -- If we can't get IP, just allow the request
      Just ip
        | isLocalIP ip -> app req k
        | otherwise -> do
          skip <- exempt ip
          allowed <- if skip then pure True else checkRateLimit ip now
          if allowed
            then app req k
            else k tooManyRequestsResponse
  | otherwise = app req k  -- Skip rate limiting for non-resource paths

-- | "a.b.c.d" or "::ffff:a.b.c.d", the form rpc_server_ip uses.
-- ponytail: IPv4 only; the RPC servers have no IPv6 addresses.
parseIPv4 :: ByteString -> Maybe IP
parseIPv4 s = case traverse (readMaybe @Int . decodeUtf8) (BS.split 46 v4) of
  Just [ a, b, c, d ]
    | all (\x -> x >= 0 && x <= 255) [ a, b, c, d ] -> Just
      (IPv4 (tupleToHostAddress ( fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d )))
  _ -> Nothing
  where
    v4 = fromMaybe s (BS.stripPrefix "::ffff:" s)

-- | Loopback, private and link-local addresses, which the official client
-- exempts from flood control (HTTPServer.localNetworkPattern).
isLocalIP :: IP -> Bool
isLocalIP (IPv4 ha) = localV4 (hostAddressToTuple ha)
isLocalIP (IPv6 ha) = case hostAddress6ToTuple ha of
  ( 0, 0, 0, 0, 0, 0, 0, 1 ) -> True
  -- IPv4-mapped (::ffff:a.b.c.d), as seen on dual-stack sockets
  ( 0, 0, 0, 0, 0, 0xffff, hi, lo ) -> localV4 ( byte hi 8, byte hi 0, byte lo 8, byte lo 0 )
  ( w, _, _, _, _, _, _, _ ) -> (w .&. 0xfe00) == 0xfc00 -- fc00::/7
  where
    byte w n = fromIntegral (w `shiftR` n)

localV4 :: ( Word8, Word8, Word8, Word8 ) -> Bool
localV4 = \case
  ( 127, _, _, _ ) -> True
  ( 10, _, _, _ ) -> True
  ( 192, 168, _, _ ) -> True
  ( 172, b, _, _ ) -> b >= 16 && b <= 31
  ( 169, 254, _, _ ) -> True
  _ -> False

-- | Count a hit for the IP; False if the request should be blocked.
checkRateLimit :: IP -> Double -> IO Bool
checkRateLimit ip now = atomicModifyIORef' globalIPMap $ \m0 ->
  let
    -- ponytail: O(n) prune once the map is large; fine for the few hundred IPs a client sees per minute
    m = if HashMap.size m0 > 1000
          then HashMap.filter ((> now - timeWindow) . lastHit) m0
          else m0
  in 
    case HashMap.lookup ip m of
      Just r
        | bannedUntil r > now -> ( m, False ) -- blocked requests are not counted, as in the official client
      old -> let
          r0      = fromMaybe (IPRecord 0 now 0) old
          hits    = max 0 (hitCount r0 - floor (now - lastHit r0)) + 1
          blocked = hits > floodLimit
          r       = IPRecord { hitCount    = hits
                             , lastHit     = now
                             , bannedUntil = if blocked then now + blockSeconds else bannedUntil r0
                             }
        in 
          ( HashMap.insert ip r m, not blocked )

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
                         Histogram.observe (endTime - startTime) (statsLatency statsEnv))

withHentaiHeaders :: Middleware
withHentaiHeaders app req k = app req (k . mapResponseHeaders addHeaders)
  where
    addHeaders headers@(map fst -> names)
      = filter ((`notElem` names) . fst) hentaiHeader ++ headers

