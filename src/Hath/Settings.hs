-- | The settings the RPC server assigns to this client.
module Hath.Settings
  ( HathSettings(..)
  , defaultHathSettings
  , parseSettings
  , settingNumber
  , maxConnections
  ) where

import qualified Data.ByteString.Char8 as BS

import           Relude

data HathSettings
  = HathSettings
  { clientHost :: !ByteString
  , clientPort :: !Int
    -- ^ The port we must listen on.
  , throttleBytes :: !Int64
    -- ^ Bandwidth limit. Nothing enforces it; it sets 'maxConnections'.
  , diskLimitBytes :: !Int64
    -- ^ Cache size set on the H\@H settings page; the Filesystem backend evicts to stay under it.
  , diskRemainingBytes :: !Int64
  , rpcServerIPs :: ![ ByteString ]
    -- ^ As sent by the server, e.g. @::ffff:192.0.2.10@. These are exempt from flood control.
  , maxConnectionsOverride :: !Int
    -- ^ @max_connections@: when above 0, 'maxConnections' is this.
  , maxAllowedFileSize :: !Int
    -- ^ @max_allowed_filesize@: a cache miss larger than this is not fetched.
  }
  deriving ( Show )

defaultHathSettings :: HathSettings
defaultHathSettings
  = HathSettings
  { clientHost = ""
  , clientPort = 0
  , throttleBytes = 0
  , diskLimitBytes = 0
  , diskRemainingBytes = 0
  , rpcServerIPs = []
  , maxConnectionsOverride = 0
    -- the official client's default (Settings.java:58)
  , maxAllowedFileSize = 1073741824
  }

-- | Apply @name=value@ lines onto the settings we have. @client_settings@ only
-- carries what changed, so a refresh must leave the other fields alone. Lines
-- we do not know, or whose number does not parse, are skipped.
parseSettings :: HathSettings -> [ ByteString ] -> HathSettings
parseSettings = foldl' apply
  where
    apply settings line = case ( name, readMaybe @Int64 (BS.unpack value) ) of
      ( "host", _ ) -> settings { clientHost = value }
      ( "port", Just n ) -> settings { clientPort = fromIntegral n }
      ( "throttle_bytes", Just n ) -> settings { throttleBytes = n }
      ( "disklimit_bytes", Just n ) -> settings { diskLimitBytes = n }
      ( "diskremaining_bytes", Just n ) -> settings { diskRemainingBytes = n }
      ( "rpc_server_ip", _ ) -> settings { rpcServerIPs = BS.split ';' value }
      ( "max_connections", Just n ) -> settings { maxConnectionsOverride = fromIntegral n }
      ( "max_allowed_filesize", Just n ) -> settings { maxAllowedFileSize = fromIntegral n }
      _ -> settings
      where
        ( name, value ) = nameValue line

-- | How many connections we take at once (Settings.getMaxConnections): what
-- @max_connections@ says, or else 20 and one more for every 10,000 bytes/s of
-- the throttle, up to 500.
maxConnections :: HathSettings -> Int
maxConnections settings
  | maxConnectionsOverride settings > 0 = maxConnectionsOverride settings
  | otherwise = 20 + fromIntegral (min 480 (throttleBytes settings `quot` 10000))

-- | The number one of a reply's @name=value@ lines gives, such as @server_time@.
settingNumber :: ByteString -> [ ByteString ] -> Maybe Int64
settingNumber wanted reply
  = listToMaybe
    [ n
    | ( name, value ) <- map nameValue reply
    , name == wanted
    , Just n <- [ readMaybe (BS.unpack value) ]
    ]

nameValue :: ByteString -> ( ByteString, ByteString )
nameValue = second (BS.drop 1) . BS.span (/= '=')
