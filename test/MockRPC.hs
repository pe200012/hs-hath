-- | A stand-in for the H\@H RPC server, which also plays the image server that
-- files are fetched from.
module MockRPC ( MockRPC(..), defaultMockRPC, withMockRPC ) where

import qualified Data.ByteString.Char8    as BS
import qualified Data.List                as List

import           Hath.Protocol            ( sha1Hex )

import qualified Network.HTTP.Types       as HTTP
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp

import           Relude

import           System.Environment       ( setEnv )

data MockRPC
  = MockRPC
  { mockFileContent :: ByteString
    -- ^ what the image server sends, whichever file is asked for
  , mockCertificate :: ByteString
    -- ^ the reply to get_cert
  , mockClientKey   :: ByteString
    -- ^ the key OK replies are signed with
  }

defaultMockRPC :: MockRPC
defaultMockRPC
  = MockRPC
  { mockFileContent = "TEST_FILE_CONTENT"
  , mockCertificate = "FAIL\nNo test certificate available"
  , mockClientKey   = "test-secret-key-12345"
  }

-- | Run an action while @HATH_RPC_HOST@ names a mock server on a free port.
withMockRPC :: MockRPC -> IO a -> IO a
withMockRPC mock action = Warp.testWithApplication (pure (application mock)) $ \port -> do
  setEnv "HATH_RPC_HOST" ("localhost:" <> show port)
  action

application :: MockRPC -> Wai.Application
application mock req respond = respond $ case ( Wai.rawPathInfo req, parameter "act" ) of
  ( "/16/rpc", Just act ) -> plain (rpcReply act)
  ( "/file", _ ) -> Wai.responseLBS
    HTTP.status200
    -- image servers always send the length
    [ ( "Content-Length", show (BS.length (mockFileContent mock)) ) ]
    (fromStrict (mockFileContent mock))
  _ -> Wai.responseLBS HTTP.status404 [ ( "Content-Type", "text/plain" ) ] "Not Found"
  where
    parameter name = join (List.lookup name (Wai.queryString req))

    plain = Wai.responseLBS HTTP.status200 [ ( "Content-Type", "text/plain; charset=utf-8" ) ] . fromStrict

    -- we are the image server too, at whatever address the client reached us
    ownAddress = fromMaybe "localhost" (Wai.requestHeaderHost req)

    rpcReply = \case
      -- the PKCS#12 file itself, never signed
      "get_cert" -> mockCertificate mock
      "client_login" -> signed
        [ "host=127.0.0.1"
        , "port=8080"
        , "throttle_bytes=0"
        , "disklimit_bytes=107374182400"
        , "diskremaining_bytes=107374182400"
        ]
      "client_settings" -> signed [ "host=127.0.0.1", "port=8080" ]
      "srfetch" -> signed [ "http://" <> ownAddress <> "/file" ]
      act
        | act `elem` answeredOK -> signed []
        | otherwise -> "FAIL\nUnknown action: " <> act

    answeredOK
      = [ "server_stat", "still_alive", "client_start", "client_stop", "get_blacklist", "dlfetch", "dlfails" ]

    -- the 1.6.6 server signs every OK reply with the client key
    signed payload
      = BS.unlines
        (("OK-1-" <> sha1Hex ("1-" <> mockClientKey mock <> foldMap ("\n" <>) payload)) : payload)
