-- | The settings the RPC server assigns to this client.
module Hath.Settings ( HathSettings(..), defaultHathSettings, parseSettings ) where

import qualified Data.ByteString.Char8 as BS

import           Relude

data HathSettings
  = HathSettings
  { clientHost         :: !ByteString
  , clientPort         :: !Int
    -- ^ The port we must listen on.
  , throttleBytes      :: !Int64
    -- ^ Bandwidth limit. Recorded, but nothing enforces it.
  , diskLimitBytes     :: !Int64
    -- ^ Cache size set on the H\@H settings page; the Filesystem backend evicts to stay under it.
  , diskRemainingBytes :: !Int64
  , rpcServerIPs       :: ![ ByteString ]
    -- ^ As sent by the server, e.g. @::ffff:192.0.2.10@. These are exempt from flood control.
  }
  deriving ( Show )

defaultHathSettings :: HathSettings
defaultHathSettings
  = HathSettings
  { clientHost         = ""
  , clientPort         = 0
  , throttleBytes      = 0
  , diskLimitBytes     = 0
  , diskRemainingBytes = 0
  , rpcServerIPs       = []
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
      _ -> settings
      where
        ( name, rest ) = BS.span (/= '=') line

        value          = BS.drop 1 rest
