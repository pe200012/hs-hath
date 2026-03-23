{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Mock RPC server for testing hs-hath client without connecting to real H@H network.
--
-- Usage:
--   1. Start the mock server with 'withMockRPC'
--   2. Set HATH_RPC_HOST environment variable to "localhost"
--   3. Run client code against the mock server
module MockRPC
  ( withMockRPC
  , MockRPCConfig(..)
  , defaultMockConfig
  , startMockRPC
  , stopMockRPC
  ) where

import           Control.Concurrent       ( ThreadId, forkIO, killThread )
import           Control.Concurrent.MVar  ( MVar, newEmptyMVar, putMVar, takeMVar )
import           Control.Exception        ( SomeException, catch, throwIO )

import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text                ( Text )
import qualified Data.Text                as T

import qualified Network.HTTP.Types       as HTTP
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp

import           Relude                   hiding ( newEmptyMVar, putMVar, takeMVar )

import           Utils                    ( hash )

-- | Configuration for mock RPC server behavior
data MockRPCConfig
  = MockRPCConfig
  { mockServerStatResponse :: ByteString  -- ^ Response for server_stat
  , mockClientLoginResponse :: ByteString  -- ^ Response for client_login
  , mockStillAliveResponse :: ByteString  -- ^ Response for still_alive
  , mockClientStartResponse :: ByteString  -- ^ Response for client_start
  , mockClientStopResponse :: ByteString  -- ^ Response for client_stop
  , mockGetCertResponse :: ByteString  -- ^ Response for get_cert (or error)
  , mockClientSettingsResponse :: ByteString -- ^ Response for client_settings
  , mockFileContent :: ByteString -- ^ Content for file downloads
  , mockPort :: Int         -- ^ Port to run mock server on
  , mockClientKey :: ByteString -- ^ Key OK replies are signed with (1.6.6 RPC)
  }

-- | Default mock configuration with valid responses
defaultMockConfig :: MockRPCConfig
defaultMockConfig
  = MockRPCConfig
  { mockServerStatResponse = "OK"
  , mockClientLoginResponse = BS.unlines
      [ "OK"
      , "host=127.0.0.1"
      , "port=8080"
      , "throttle_bytes=0"
      , "disklimit_bytes=107374182400"
      , "diskremaining_bytes=107374182400"
      ]
  , mockStillAliveResponse = "OK"
  , mockClientStartResponse = "OK"
  , mockClientStopResponse = "OK"
  , mockGetCertResponse = "FAIL\nNo test certificate available"
  , mockClientSettingsResponse = BS.unlines [ "OK", "host=127.0.0.1", "port=8080" ]
  , mockFileContent = "TEST_FILE_CONTENT"
  , mockPort = 19800
  , mockClientKey = "test-secret-key-12345"
  }

-- | Mock RPC application
mockRPCApp :: MockRPCConfig -> Wai.Application
mockRPCApp config req respond = do
  let path  = Wai.rawPathInfo req
      query = Wai.queryString req
      act   = join $ snd <$> find ((== "act") . fst) query  -- Extract "act" parameter

  case ( path, act ) of
    -- RPC endpoint
    ( "/16/rpc", Just action ) -> handleRPCAction config action respond
    ( "/file", _ ) -> handleDownload config respond
    _ -> respond $ notFoundResponse

-- | Handle RPC actions based on the 'act' parameter
handleRPCAction :: MockRPCConfig
                -> ByteString
                -> (Wai.Response -> IO Wai.ResponseReceived)
                -> IO Wai.ResponseReceived
handleRPCAction config action respond = respond $ okResponse $ case action of
  "get_cert" -> mockGetCertResponse config  -- the raw PKCS#12 file, never signed
  "server_stat" -> sign $ mockServerStatResponse config
  "client_login" -> sign $ mockClientLoginResponse config
  "still_alive" -> sign $ mockStillAliveResponse config
  "client_start" -> sign $ mockClientStartResponse config
  "client_stop" -> sign $ mockClientStopResponse config
  "client_settings" -> sign $ mockClientSettingsResponse config
  "get_blacklist" -> sign "OK"  -- Empty blacklist
  "dlfetch" -> sign "OK"  -- No download URLs
  "dlfails" -> sign "OK"  -- Acknowledge failures
  "srfetch" -> sign
    $ "OK\nhttp://localhost:" <> encodeUtf8 (show (mockPort config) :: Text) <> "/file"
  _ -> "FAIL\nUnknown action: " <> action
  where
    -- the 1.6.6 server signs every OK reply with the client key
    sign body = case BS.lines body of
      "OK" : rest -> BS.unlines
        (("OK-1-" <> hash ("1-" <> mockClientKey config <> foldMap ("\n" <>) rest)) : rest)
      _ -> body

-- | Handle download endpoint
handleDownload
  :: MockRPCConfig -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleDownload config respond
  = respond
  $ Wai.responseLBS
    HTTP.status200
    [ ( "Content-Length", show (BS.length (mockFileContent config)) ) ] -- image servers always send it
    (LBS.fromStrict (mockFileContent config))

-- | Create a 200 OK response with given body
okResponse :: ByteString -> Wai.Response
okResponse body
  = Wai.responseLBS
    HTTP.status200
    [ ( "Content-Type", "text/plain; charset=utf-8" ) ]
    (LBS.fromStrict body)

-- | Create a 404 Not Found response
notFoundResponse :: Wai.Response
notFoundResponse = Wai.responseLBS HTTP.status404 [ ( "Content-Type", "text/plain" ) ] "Not Found"

-- | Start mock RPC server in background thread
startMockRPC :: MockRPCConfig -> IO ( ThreadId, MVar () )
startMockRPC config = do
  ready <- newEmptyMVar
  tid <- forkIO $ do
    let settings
          = Warp.setPort (mockPort config)
          $ Warp.setBeforeMainLoop (putMVar ready ())
          $ Warp.defaultSettings
    Warp.runSettings settings (mockRPCApp config)
  takeMVar ready  -- Wait for server to be ready
  return ( tid, ready )

-- | Stop mock RPC server
stopMockRPC :: ThreadId -> IO ()
stopMockRPC = killThread

-- | Run an action with a mock RPC server
-- The server is started before the action and stopped after.
withMockRPC :: MockRPCConfig -> IO a -> IO a
withMockRPC config action = do
  ( tid, _ ) <- startMockRPC config
  result <- action `finally'` stopMockRPC tid
  return result
  where
    finally' :: IO a -> IO () -> IO a
    finally' io cleanup = do
      result <- io `catch` (\(e :: SomeException) -> cleanup >> throwIO e)
      cleanup
      return result

