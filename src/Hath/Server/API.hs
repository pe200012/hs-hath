-- | The HTTP interface this client offers to readers and to the RPC server:
-- what a request path asks for, read as the official client reads it
-- (HTTPResponse.parseRequest), and the server commands it understands.
module Hath.Server.API
  ( Route(..)
  , route
  , ServerCommand(..)
  , ProxyTest(..)
  , parseServerCommand
  ) where

import qualified Data.ByteString.Char8 as BSC
import           Data.List             ( dropWhileEnd )
import qualified Data.Map.Strict       as Map
import qualified Data.Text             as T

import           Network.HTTP.Types    ( Status, status400, status403, status404 )

import           Relude

data Route
  = Favicon
  | Robots
  | File !Text !Text
    -- ^ @\/h\/\<file id>\/\<options>\/\<file name>@: a file for a reader. The
    -- file name is optional.
  | ServerCmd !Text !Text !Int !Text
    -- ^ @\/servercmd\/\<command>\/\<additional>\/\<time>\/\<key>@: a command
    -- from the RPC server
  | Test !Int !Int64 !Text
    -- ^ @\/t\/\<size>\/\<time>\/\<key>\/\<anything>@: a speed test, for the RPC
    -- server or another client. The last part is optional.
  | Metrics
  | AdminSettings
  | Refuse !Status
-- ^ a path the official client answers with this error. Where its numbers are
-- not numbers, it drops the connection instead.

-- | The route of a request path. Like the official client, we do not
-- percent-decode the path, except @%3d@ (=): the keys of server commands and
-- speed tests are computed over it as it is. Empty parts at its end do not count.
route :: ByteString -> Route
route path = case dropWhileEnd T.null (T.splitOn "/" (T.replace "%3d" "=" (decodeUtf8 path))) of
  [ "", "favicon.ico" ] -> Favicon
  [ "", "robots.txt" ] -> Robots
  [ "", "metrics" ] -> Metrics
  [ "", "admin", "settings" ] -> AdminSettings
  "" : "h" : fileId : options : _ -> File fileId options
  "" : "h" : _ -> Refuse status400
  "" : "servercmd" : command : additional : time : commandKey : _ -> maybe
    (Refuse status403)
    (\t -> ServerCmd command additional t commandKey)
    (number (encodeUtf8 time))
  "" : "servercmd" : _ -> Refuse status403
  "" : "t" : size : time : testKey : _ -> fromMaybe (Refuse status400)
    $ Test <$> number (encodeUtf8 size) <*> number (encodeUtf8 time) <*> pure testKey
  "" : "t" : _ -> Refuse status400
  _ -> Refuse status404

-- | What the RPC server may tell us to do through @\/servercmd@, with what each needs.
data ServerCommand
  = StillAlive
    -- ^ are you there?
  | ThreadedProxyTest !ProxyTest
    -- ^ download from another client and report how it went
  | SpeedTest !Int
    -- ^ send this many bytes
  | RefreshSettings
    -- ^ your settings have changed
  | StartDownloader
    -- ^ there is a gallery in your download queue
  | RefreshCerts
    -- ^ fetch your new certificate
  deriving ( Eq, Show )

-- | Downloads from another client's @\/t\/@ route, to time them.
data ProxyTest
  = ProxyTest { proxyTestCount :: !Int, proxyTestSize :: !Int64, proxyTestURL :: !String }
  deriving ( Eq, Show )

-- | A command by its name, in any case, with the parameters it takes
-- (HTTPResponse.processRemoteAPICommand). Nothing for an unknown name or a
-- parameter it cannot read: the official client answers both with INVALID_COMMAND.
parseServerCommand :: Text -> Map ByteString ByteString -> Maybe ServerCommand
parseServerCommand name params = case T.toLower name of
  "still_alive" -> Just StillAlive
  "threaded_proxy_test" -> ThreadedProxyTest <$> parseProxyTest params
  "speed_test" -> SpeedTest <$> maybe (Just 1000000) number (Map.lookup "testsize" params)
  "refresh_settings" -> Just RefreshSettings
  "start_downloader" -> Just StartDownloader
  "refresh_certs" -> Just RefreshCerts
  _ -> Nothing

parseProxyTest :: Map ByteString ByteString -> Maybe ProxyTest
parseProxyTest params = do
  hostname <- param "hostname"
  port <- param "port" >>= number @Int
  size <- param "testsize" >>= number
  count <- param "testcount" >>= number
  time <- param "testtime" >>= number @Int64
  testKey <- param "testkey"
  let protocol = fromMaybe "http" (param "protocol")
  pure
    ProxyTest
    { proxyTestCount = count
    , proxyTestSize  = size
    , proxyTestURL   = BSC.unpack
        $ mconcat
          [ protocol
          , "://"
          , hostname
          , ":"
          , show port
          , "/t/"
          , show size
          , "/"
          , show time
          , "/"
          , testKey
          , "/0"
          ]
    }
  where
    param name = Map.lookup name params

-- | A number as Java's Integer.parseInt reads it: digits, perhaps signed, and nothing else.
number :: Num a => ByteString -> Maybe a
number s = case BSC.readInt s of
  Just ( n, "" ) -> Just (fromIntegral n)
  _ -> Nothing
