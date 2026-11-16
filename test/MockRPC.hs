-- | A stand-in for the H\@H RPC server, which also plays the image server that
-- files are fetched from.
module MockRPC ( MockRPC(..), defaultMockRPC, withMockRPC ) where

import qualified Data.ByteString.Char8    as BS
import qualified Data.List                as List
import           Data.Time.Clock.System   ( SystemTime(systemSeconds), getSystemTime )

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
  , mockClockAhead  :: Int64
    -- ^ how far its clock is ahead of ours. A request signed more than a
    -- minute off it is answered KEY_EXPIRED, except server_stat, which
    -- tells the time.
  , mockOutages     :: Int
    -- ^ how many RPC requests it answers with a 500 before it works
  }

defaultMockRPC :: MockRPC
defaultMockRPC
  = MockRPC { mockFileContent = "TEST_FILE_CONTENT"
            , mockCertificate = "FAIL\nNo test certificate available"
            , mockClientKey   = "test-secret-key-12345"
            , mockClockAhead  = 0
            , mockOutages     = 0
            }

-- | Run an action while @HATH_RPC_HOST@ names a mock server on a free port.
withMockRPC :: MockRPC -> IO a -> IO a
withMockRPC mock action = do
  outagesLeft <- newIORef (mockOutages mock)
  Warp.testWithApplication (pure (application mock outagesLeft)) $ \port -> do
    setEnv "HATH_RPC_HOST" ("localhost:" <> show port)
    action

application :: MockRPC -> IORef Int -> Wai.Application
application mock outagesLeft req respond = case ( Wai.rawPathInfo req, parameter "act" ) of
  ( "/16/rpc", Just act ) -> do
    down <- atomicModifyIORef' outagesLeft $ \n -> ( n - 1, n > 0 )
    clock <- (+ mockClockAhead mock) . systemSeconds <$> getSystemTime
    respond
      $ if down
        then Wai.responseLBS HTTP.status500 [] ""
        else Wai.responseLBS HTTP.status200 [] $ fromStrict $ rpcReply clock act
  ( "/file", _ ) -> respond
    $ Wai.responseLBS
      HTTP.status200
      -- image servers always send the length
      [ ( "Content-Length", show (BS.length (mockFileContent mock)) ) ]
      (fromStrict (mockFileContent mock))
  _ -> respond $ Wai.responseLBS HTTP.status404 [ ( "Content-Type", "text/plain" ) ] "Not Found"
  where
    parameter name = join (List.lookup name (Wai.queryString req))

    -- we are the image server too, at whatever address the client reached us
    ownAddress     = fromMaybe "localhost" (Wai.requestHeaderHost req)

    signedAt clock
      = maybe
        False
        (\time -> abs (time - clock) <= 60)
        (readMaybe . BS.unpack =<< parameter "acttime")

    rpcReply clock = \case
      "server_stat" -> signed [ "server_time=" <> show clock ]
      act
        | not (signedAt clock) -> "KEY_EXPIRED"
        | otherwise -> reply act

    reply          = \case
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
      = [ "still_alive", "client_start", "client_stop", "get_blacklist", "dlfetch", "dlfails" ]

    -- the 1.6.6 server signs every OK reply with the client key
    signed payload
      = BS.unlines
        (("OK-1-" <> sha1Hex ("1-" <> mockClientKey mock <> foldMap ("\n" <>) payload)) : payload)
