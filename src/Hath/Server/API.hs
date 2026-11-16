{-# LANGUAGE DataKinds #-}

-- | The HTTP interface this client offers to readers and to the RPC server.
module Hath.Server.API ( API, api, Body, ServerCommand(..), renderServerCommand ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List            as List

import           Network.HTTP.Media   ( (//), (/:) )

import           Relude

import           Servant

-- | What the RPC server may tell us to do through @\/servercmd@.
data ServerCommand
  = StillAlive        -- ^ are you there?
  | ThreadedProxyTest -- ^ download from another client and report how it went
  | SpeedTest         -- ^ send this many bytes
  | RefreshSettings   -- ^ your settings have changed
  | StartDownloader   -- ^ there is a gallery in your download queue
  | RefreshCerts      -- ^ fetch your new certificate
  deriving ( Show, Eq )

serverCommandNames :: [ ( Text, ServerCommand ) ]
serverCommandNames
  = [ ( "still_alive", StillAlive )
    , ( "threaded_proxy_test", ThreadedProxyTest )
    , ( "speed_test", SpeedTest )
    , ( "refresh_settings", RefreshSettings )
    , ( "start_downloader", StartDownloader )
    , ( "refresh_certs", RefreshCerts )
    ]

instance FromHttpApiData ServerCommand where
  parseUrlPiece name = maybeToRight ("Invalid command: " <> name) (List.lookup name serverCommandNames)

-- | The name of a command on the wire. Its signature is computed over this.
renderServerCommand :: ServerCommand -> Text
renderServerCommand command = maybe "" fst (find ((== command) . snd) serverCommandNames)

-- | The content type of a speed test body: whatever the tester accepts.
data SpeedTest

instance MimeRender SpeedTest ByteString where
  mimeRender _ = LBS.fromStrict

instance Accept SpeedTest where
  contentTypes _
    = "text" // "html" /: ( "charset", "iso-8859-1" )
    :| [ "image" // "gif", "image" // "jpeg", "*" // "*" /: ( "q", ".2" ) ]

-- | A body of known length, sent as it is produced.
type Body = Headers '[ Header "Content-Length" Int ] (SourceIO ByteString)

-- floskell-disable
type API
  =    "favicon.ico" :> Get '[ PlainText ] NoContent
  :<|> "robots.txt" :> Get '[ PlainText ] Text
       -- a file: /h/<file id>/<options>/<file name>
  :<|> "h" :> Capture "info" Text :> Capture "opts" Text :> Capture "filename" Text :> RawM
  :<|> "metrics" :> Get '[ PlainText ] Text
  :<|> "servercmd"
       :> Capture "command" ServerCommand
       :> Capture "additional" Text
       :> Capture "time" Int
       :> Capture "key" Text
       :> StreamGet NoFraming OctetStream Body
       -- a speed test: /t/<size>/<time>/<key>/<anything>
  :<|> "t"
       :> Capture "testsize" Int
       :> Capture "testtime" Int64
       :> Capture "testkey" Text
       :> Capture "nothing" Text
       :> StreamGet NoFraming SpeedTest Body
  :<|> "admin" :> "settings" :> Get '[ PlainText ] Text
       -- everything else: 200 for HEAD, 404 otherwise
  :<|> Raw
-- floskell-enable

api :: Proxy API
api = Proxy
