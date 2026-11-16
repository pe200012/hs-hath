-- | Flood control as the official client does it (HTTPServer.FloodControlEntry):
-- every request adds one hit, hits drain at one per second, and more than
-- 'floodLimit' hits blocks the address for 'blockSeconds'.
module Hath.Server.FloodControl
  ( -- * Addresses
    IP(..)
  , sockAddrIP
  , parseIPv4
  , isLocalIP
    -- * The pure rule
  , IPRecord(..)
  , IPMap
  , registerHit
    -- * Shared state
  , FloodControl
  , newFloodControl
  , checkRateLimit
  ) where

import           Data.Bits           ( (.&.), shiftR )
import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HashMap

import           Network.Socket      ( HostAddress
                                     , HostAddress6
                                     , SockAddr(..)
                                     , hostAddress6ToTuple
                                     , hostAddressToTuple
                                     , tupleToHostAddress
                                     )

import           Relude

data IP = IPv4 !HostAddress | IPv6 !HostAddress6
  deriving ( Eq, Ord, Show, Generic )

instance Hashable IP

-- | The address of a peer, without its port.
sockAddrIP :: SockAddr -> Maybe IP
sockAddrIP (SockAddrInet _ address) = Just $ IPv4 address
sockAddrIP (SockAddrInet6 _ _ address _) = Just $ IPv6 address
sockAddrIP _ = Nothing

-- | @a.b.c.d@ or @::ffff:a.b.c.d@, the form @rpc_server_ip@ uses.
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
isLocalIP (IPv4 address) = localV4 (hostAddressToTuple address)
isLocalIP (IPv6 address) = case hostAddress6ToTuple address of
  ( 0, 0, 0, 0, 0, 0, 0, 1 )        -> True
  -- IPv4-mapped (::ffff:a.b.c.d), as seen on dual-stack sockets
  ( 0, 0, 0, 0, 0, 0xffff, hi, lo ) -> localV4 ( byte hi 8, byte hi 0, byte lo 8, byte lo 0 )
  ( w, _, _, _, _, _, _, _ )        -> (w .&. 0xfe00) == 0xfc00 -- fc00::/7
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

floodLimit :: Int
floodLimit = 10

blockSeconds :: Double
blockSeconds = 60

-- | Entries idle for this long carry no state worth keeping: their hits have
-- drained and any block has expired.
idleSeconds :: Double
idleSeconds = 60

-- | Times are monotonic seconds.
data IPRecord
  = IPRecord { hitCount    :: !Int
               -- ^ hits left after draining
             , lastHit     :: !Double
               -- ^ when the last hit was counted
             , bannedUntil :: !Double
             }
  deriving ( Eq, Show )

type IPMap = HashMap IP IPRecord

-- | Count a request from an address at a time. The Bool says whether it may
-- proceed. A request from a blocked address is refused without being counted,
-- as in the official client.
registerHit :: Double -> IP -> IPMap -> ( IPMap, Bool )
registerHit now ip records = case HashMap.lookup ip live of
  Just record
    | bannedUntil record > now -> ( live, False )
  known       -> let
      previous = fromMaybe (IPRecord 0 now 0) known
      hits     = max 0 (hitCount previous - floor (now - lastHit previous)) + 1
      blocked  = hits > floodLimit
      record
        = IPRecord { hitCount    = hits
                   , lastHit     = now
                   , bannedUntil = if blocked
                       then now + blockSeconds
                       else bannedUntil previous
                   }
    in 
      ( HashMap.insert ip record live, not blocked )
  where
    -- ponytail: O(n) prune once the map is large; fine for the few hundred IPs a client sees per minute
    live
      | HashMap.size records > 1000 = HashMap.filter ((> now - idleSeconds) . lastHit) records
      | otherwise = records

-- | The hit records of one server.
newtype FloodControl = FloodControl (IORef IPMap)

newFloodControl :: IO FloodControl
newFloodControl = FloodControl <$> newIORef HashMap.empty

-- | 'registerHit' on the shared records; False if the request should be refused.
checkRateLimit :: FloodControl -> IP -> Double -> IO Bool
checkRateLimit (FloodControl records) ip now = atomicModifyIORef' records (registerHit now ip)
