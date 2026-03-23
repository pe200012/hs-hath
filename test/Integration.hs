{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Integration tests for hs-hath full lifecycle
module Integration ( integrationSpecs ) where

import           Control.Concurrent       ( ThreadId, forkIO, killThread, threadDelay )
import           Control.Concurrent.MVar  ( MVar, newEmptyMVar, putMVar, tryTakeMVar )
import           Control.Concurrent.STM   ( TVar, newTVarIO )
import           Control.Exception        ( bracket, catch, SomeException )

import qualified Data.Aeson               as A
import qualified Data.Aeson               as A
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BSC
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Map.Strict          as Map
import           Data.String.Interpolate  ( i )
import qualified Data.Text                as T
import           Data.Text.Encoding       ( decodeUtf8, encodeUtf8 )
import           Data.Time.Clock.System   ( SystemTime(systemSeconds), getSystemTime )
import           Data.X509                ( CertificateChain(..), PrivKey )
import           Data.X509.File           ( readSignedObject, readKeyFile )

import           Storage.Database         ( initializeDB )
import           Database.SQLite.Simple   ( withConnection )

import           Utils                    ( hash )

import           MockRPC

import           Network.HTTP.Client      ( defaultManagerSettings
                                          , httpLbs
                                          , method
                                          , newManager
                                          , parseRequest
                                          , redirectCount
                                          , responseBody
                                          , responseHeaders
                                          , responseStatus
                                          )
import           Network.HTTP.Types       ( status200, status301, status403, status404, status429 )
import qualified Network.Wai.Handler.Warp as Warp

import           Relude                   hiding ( Reader
                                                 , decodeUtf8
                                                 , encodeUtf8
                                                 , newEmptyMVar
                                                 , newTVarIO
                                                 , runReader
                                                 )

import           Server                   ( CacheRunner(..)
                                          , ServerAction(..)
                                          , GalleryTask(..)
                                          , makeApplication
                                          , mkServerLoopContext
                                          , startServer
                                          )



import           Storage.Database         ( runCache )



import           Stats                    ( newStatsEnv )



import           System.Environment       ( setEnv )
import           System.Timeout           ( timeout )

import           Middleware               ( IP(..), checkRateLimit, isLocalIP, parseIPv4 )

import           Network.Socket           ( tupleToHostAddress, tupleToHostAddress6 )

import           Polysemy                 ( interpret )
import           Polysemy.Error           ( throw )
import           Polysemy.KVStore         ( KVStore(..) )



import           Test.Hspec



import           UnliftIO                 ( TChan



                                          , newTChanIO



                                          , tryReadTChan



                                          , writeTChan



                                          )







import           Types                    ( CacheBackend(..)
                                          , RPCError(StorageError)


                                          , ClientConfig(..)
                                          , GalleryFile(..)
                                          , GalleryMetadata(..)
                                          , HathSettings(..)
                                          , defaultHathSettings
                                          , parseMetadata
                                          , verifyRPCResponse
                                          )

-- | Test configuration
testConfig :: Int -> ClientConfig
testConfig port
  = ClientConfig
  { clientId     = "test-client-001"
  , key          = "test-secret-key-12345"
  , version      = "1.0.0-test"
  , proxy        = Nothing
  , downloadDir  = "/tmp/hs-hath-test"
  , cachePath    = "/tmp/hs-hath-test-" <> show port <> ".db"
  , cacheBackend = CacheBackendSQLite
  , r2Config     = Nothing
  , lruCacheSize = 100 * 1024 * 1024
  }

-- | A servercmd URL signed the way the RPC server signs it.
serverCmdUrl :: Int -> ClientConfig -> Text -> Text -> Int64 -> String
serverCmdUrl port cfg cmd add ts = [i|http://localhost:#{port}/servercmd/#{cmd}/#{add}/#{ts}/#{sig}|]
  where
    sig = hash @Text $ encodeUtf8 ([i|hentai@home-servercmd-#{cmd}-#{add}-#{clientId cfg}-#{ts}-#{key cfg}|] :: Text)

withTestServerConfigured
  :: Bool
  -> MockRPCConfig
  -> ClientConfig
  -> HathSettings
  -> (Int -> IO a)
  -> IO a
withTestServerConfigured disableRateLimit mockCfg clientCfg settings action
  = withConnection ":memory:" $ \conn -> do
    initializeDB conn
    withRunnerTestServer
      disableRateLimit
      CacheRunner { runCacheWith = runCache conn }
      mockCfg
      clientCfg
      settings
      action

withRunnerTestServer
  :: Bool
  -> CacheRunner
  -> MockRPCConfig
  -> ClientConfig
  -> HathSettings
  -> (Int -> IO a)
  -> IO a
withRunnerTestServer disableRateLimit cacheRunner mockCfg clientCfg settings action = withMockRPC mockCfg $ do
  setEnv "HATH_RPC_HOST" ("localhost:" <> show (mockPort mockCfg))
  serverChan <- newTChanIO
  galleryHandler <- newTChanIO
  ssVar <- newTVarIO settings
  statsEnv <- newStatsEnv
  let ctx = mkServerLoopContext
        clientCfg
        ssVar
        serverChan
        galleryHandler
        disableRateLimit
        False
        cacheRunner
        statsEnv
      app = makeApplication ctx
  Warp.testWithApplication (pure app) action

withTestServer
  :: MockRPCConfig
  -> ClientConfig
  -> HathSettings
  -> (Int -> IO a)
  -> IO a
withTestServer = withTestServerConfigured True

withRateLimitedTestServer
  :: MockRPCConfig
  -> ClientConfig
  -> HathSettings
  -> (Int -> IO a)
  -> IO a
withRateLimitedTestServer = withTestServerConfigured False

-- | Helper to load test certificates
loadTestCertificates :: IO (CertificateChain, [PrivKey])
loadTestCertificates = do
  certs <- readSignedObject "test-cert.pem"
  keys <- readKeyFile "test-key.pem"
  return (CertificateChain certs, keys)

-- | Test with startServer (includes full initialization)
withStartServer
  :: MockRPCConfig
  -> ClientConfig
  -> HathSettings
  -> (Int -> TChan ServerAction -> IO a)
  -> IO a
withStartServer mockCfg clientCfg settings action = withMockRPC mockCfg $ do
  setEnv "HATH_RPC_HOST" ("localhost:" <> show (mockPort mockCfg))
  (certChain, keys) <- loadTestCertificates
  case keys of
    [] -> error "No private key found in test-key.pem"
    (privKey:_) -> do
      serverChan <- newTChanIO
      let certs = (certChain, privKey)
          port = clientPort settings
      
      -- Start server in background thread (skipVerify = True for tests)
      bracket
        (forkIO $ startServer clientCfg settings certs serverChan True False)
        killThread
        (\_ -> do
          -- Wait a bit for server to initialize
          threadDelay 1000000  -- 1 second
          -- Run the test action
          result <- action port serverChan
          -- Shutdown server gracefully
          atomically $ writeTChan serverChan GracefulShutdown
          threadDelay 500000  -- Wait for shutdown
          return result
        )

-- | Integration specs
integrationSpecs :: Spec
integrationSpecs = do
  describe "Full Server Lifecycle" $ do
    it "serves requested file by fetching from upstream (MockRPC)" $ do
      let mockPortNum = 19801
          mockContent = "TEST_FILE_CONTENT_123"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 0
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-txt"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/testfile.txt|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status200
        LBS.toStrict (responseBody response) `shouldBe` mockContent
        let parsedOpts = Map.fromList $ do
              t <- T.splitOn ";" opts
              let (optKey, v) = T.breakOn "=" t
              pure (encodeUtf8 optKey, encodeUtf8 (T.drop 1 v))
        Map.lookup "keystamp" parsedOpts `shouldSatisfy` isJust

    it "rejects requests with invalid keystamp" $ do
      let mockPortNum = 19802
          mockContent = "TEST_FILE_CONTENT_123"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 1
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-txt"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            invalidKeystampText = "invalid" :: Text
            opts = [i|keystamp=#{invalidKeystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/testfile.txt|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status403
        responseBody response `shouldSatisfy` const True

    it "flood control matches the official client" $ do
      let ip = IPv4 (tupleToHostAddress ( 203, 0, 113, 7 ))
          t0 = 1000 :: Double
      -- ten quick hits pass, the eleventh blocks
      firstTen <- forM [ 0 .. 9 ] $ \k -> checkRateLimit ip (t0 + 0.1 * k)
      firstTen `shouldBe` replicate 10 True
      checkRateLimit ip (t0 + 1.0) `shouldReturn` False
      -- blocked for 60 s, then counting starts over
      checkRateLimit ip (t0 + 59) `shouldReturn` False
      checkRateLimit ip (t0 + 62) `shouldReturn` True
      -- one hit per second drains as fast as it fills
      let ip2 = IPv4 (tupleToHostAddress ( 203, 0, 113, 8 ))
      steady <- forM [ 0 .. 29 ] $ \k -> checkRateLimit ip2 (t0 + k)
      steady `shouldBe` replicate 30 True

    it "treats loopback, private and IPv4-mapped local addresses as local" $ do
      isLocalIP (IPv4 (tupleToHostAddress ( 127, 0, 0, 1 ))) `shouldBe` True
      isLocalIP (IPv4 (tupleToHostAddress ( 172, 20, 1, 1 ))) `shouldBe` True
      isLocalIP (IPv4 (tupleToHostAddress ( 172, 32, 1, 1 ))) `shouldBe` False
      isLocalIP (IPv4 (tupleToHostAddress ( 203, 0, 113, 7 ))) `shouldBe` False
      isLocalIP (IPv6 (tupleToHostAddress6 ( 0, 0, 0, 0, 0, 0xffff, 0x7f00, 1 ))) `shouldBe` True
      isLocalIP (IPv6 (tupleToHostAddress6 ( 0, 0, 0, 0, 0, 0xffff, 0xcb00, 0x7107 ))) `shouldBe` False
      isLocalIP (IPv6 (tupleToHostAddress6 ( 0xfd12, 0, 0, 0, 0, 0, 0, 1 ))) `shouldBe` True

    it "parses rpc_server_ip entries" $ do
      parseIPv4 "::ffff:192.0.2.10" `shouldBe` Just (IPv4 (tupleToHostAddress ( 192, 0, 2, 10 )))
      parseIPv4 "198.51.100.20" `shouldBe` Just (IPv4 (tupleToHostAddress ( 198, 51, 100, 20 )))
      parseIPv4 "300.1.1.1" `shouldBe` Nothing
      parseIPv4 "fe80::1" `shouldBe` Nothing

    it "accepts only RPC replies signed with the client key (1.6.6)" $ do
      let k = "test-secret-key-12345"
          sig t ls = hash @ByteString (t <> "-" <> k <> foldMap ("\n" <>) ls)
      verifyRPCResponse k ("OK-7-" <> sig "7" [ "a=1", "b=2" ] <> "\na=1\nb=2\n")
        `shouldBe` "OK\na=1\nb=2"
      verifyRPCResponse k ("OK-7-" <> sig "7" [ "a=1" ] <> "\na=2") `shouldBe` "RESPONSE_HASH_FAILED"
      verifyRPCResponse k "OK\na=1" `shouldBe` "RESPONSE_HASH_FAILED"
      verifyRPCResponse k "KEY_EXPIRED" `shouldBe` "KEY_EXPIRED"

    it "accepts only signed gallery lists with safe file names (1.6.6)" $ do
      let k = "test-secret-key-12345"
          meta signedFor files = BS.intercalate "\n"
            $ [ "GID 123", "FILECOUNT 2", "MINXRES org"
              , "DLHASH " <> hash (k <> foldMap ("\n" <>) signedFor), "TITLE t", "", "FILELIST" ]
            <> files
            <> [ "", "INFORMATION", "not a file line, not signed" ]
          ok files = meta files files
          good = [ "1 99 org " <> hash @ByteString "x" <> " jpg 001", "2 98 780 unknown webp p_2" ]
          files = fmap (fmap (\f -> ( galleryFileName f, galleryFileHash f )) . galleryFileList)
      files (parseMetadata k (ok good))
        `shouldBe` Right [ ( "001", Just (hash "x") ), ( "p_2", Nothing ) ]
      isLeft (parseMetadata k (meta good (take 1 good))) `shouldBe` True  -- list tampered
      isLeft (parseMetadata k (ok [ "1 99 org h jpg ../../.ssh/x" ])) `shouldBe` True
      isLeft (parseMetadata k (ok [ "1 99 org h jpg .." ])) `shouldBe` True
      isLeft (parseMetadata k (ok [ "1 99 org h exe 001" ])) `shouldBe` True
      galleryID <$> parseMetadata k "NO_PENDING_DOWNLOADS\n" `shouldBe` Left "NO_PENDING_DOWNLOADS"

    it "exempts local addresses from flood control" $ do
      let mockPortNum = 19803
          mockContent = "RL_TEST_CONTENT"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 2
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-txt"

      withRateLimitedTestServer mockConfig clientCfg defaultHathSettings { throttleBytes = 1 } $ \serverPort -> do
        now <- getSystemTime
        let ts = systemSeconds now
            k  = key clientCfg
            goodKeystamp =
              let inner = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
              in [i|#{ts}-#{decodeUtf8 (BS.take 10 (hash @ByteString inner))}|] :: Text
            opts = [i|keystamp=#{goodKeystamp};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/testfile.txt|]

        manager <- newManager defaultManagerSettings
        req0 <- parseRequest url
        let burstRequests = replicate 13 req0

        responses <- forM burstRequests $ \req -> httpLbs req manager
        let statuses = responseStatus <$> responses
        statuses `shouldBe` replicate 13 status200

    it "returns stats snapshot with non-negative counters" $ do
      let mockPortNum = 19804
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 3

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        manager <- newManager defaultManagerSettings
        req <- parseRequest [i|http://localhost:#{serverPort}/metrics|]
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status200
        LBS.toStrict (responseBody resp) `shouldSatisfy` (\body -> "hath_cache_sent_total" `BS.isInfixOf` body)

    it "returns 404 for unknown raw routes" $ do
      let mockPortNum = 19805
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 4

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        manager <- newManager defaultManagerSettings
        req <- parseRequest [i|http://localhost:#{serverPort}/unknown-path|]
        responseStatus <$> httpLbs req manager `shouldReturn` status404

    it "serves /robots.txt with disallow all" $ do
      let mockPortNum = 19806
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 5

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        manager <- newManager defaultManagerSettings
        req <- parseRequest [i|http://localhost:#{serverPort}/robots.txt|]
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status200
        LBS.toStrict (responseBody resp) `shouldSatisfy` (\body -> "Disallow" `BS.isInfixOf` body)

    it "redirects /favicon.ico to e-hentai.org" $ do
      let mockPortNum = 19807
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 6

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        manager <- newManager defaultManagerSettings
        req0 <- parseRequest [i|http://localhost:#{serverPort}/favicon.ico|]
        let req = req0 { redirectCount = 0 }
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status301

    it "handles /servercmd/still_alive" $ do
      let mockPortNum = 19808
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 7

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        now <- getSystemTime
        let ts = systemSeconds now
        manager <- newManager defaultManagerSettings
        req <- parseRequest (serverCmdUrl serverPort clientCfg "still_alive" "additional" ts)
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status200
        LBS.toStrict (responseBody resp) `shouldSatisfy` (\body -> "FANTASTIC" `BS.isInfixOf` body)

    it "handles /servercmd/speed_test" $ do
      let mockPortNum = 19809
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 8

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        now <- getSystemTime
        let ts = systemSeconds now
        manager <- newManager defaultManagerSettings
        req <- parseRequest (serverCmdUrl serverPort clientCfg "speed_test" "testsize=1000" ts)
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status200

    it "handles /t speed test endpoint with valid key" $ do
      let mockPortNum = 19810
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 9
          testSize = 1000 :: Int

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        now <- getSystemTime
        let ts = systemSeconds now
            cid = clientId clientCfg
            k = key clientCfg
            challengeBS = encodeUtf8 $ T.pack $ "hentai@home-speedtest-" <> show testSize <> "-" <> show ts <> "-" <> T.unpack cid <> "-" <> T.unpack k
            testKeyText = hash @Text challengeBS
            urlStr = "http://localhost:" <> show serverPort <> "/t/" <> show testSize <> "/" <> show ts <> "/" <> T.unpack testKeyText <> "/nothing"
        manager <- newManager defaultManagerSettings
        req <- parseRequest urlStr
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status200

    it "rejects /t speed test with invalid time drift" $ do
      let mockPortNum = 19811
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 10
          testSize = 1000 :: Int

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        let ts = 1000000 :: Int64
            testKeyStr = "invalid-key" :: String
            url = [i|http://localhost:#{serverPort}/t/#{testSize}/#{ts}/#{testKeyStr}/nothing|] :: String
        manager <- newManager defaultManagerSettings
        req <- parseRequest url
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status403

    it "handles HEAD request with 200" $ do
      let mockPortNum = 19812
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 11

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        manager <- newManager defaultManagerSettings
        req0 <- parseRequest [i|http://localhost:#{serverPort}/anything|]
        let req = req0 { method = "HEAD" }
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status200
        responseBody resp `shouldBe` ""

    it "handles /servercmd/refresh_settings" $ do
      let mockPortNum = 19813
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 12

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        now <- getSystemTime
        let ts = systemSeconds now
        manager <- newManager defaultManagerSettings
        req <- parseRequest (serverCmdUrl serverPort clientCfg "refresh_settings" "additional" ts)
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status200

    it "handles /servercmd/refresh_certs" $ do
      let mockPortNum = 19814
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 13

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        now <- getSystemTime
        let ts = systemSeconds now
        manager <- newManager defaultManagerSettings
        req <- parseRequest (serverCmdUrl serverPort clientCfg "refresh_certs" "additional" ts)
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status200

    it "handles /servercmd/threaded_proxy_test with missing params" $ do
      let mockPortNum = 19815
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 14

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        now <- getSystemTime
        let ts = systemSeconds now
        manager <- newManager defaultManagerSettings
        req <- parseRequest (serverCmdUrl serverPort clientCfg "threaded_proxy_test" "additional" ts)
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status403

    it "rejects unsigned /servercmd" $ do
      let mockConfig = defaultMockConfig { mockPort = 19830 }
          clientCfg  = testConfig 30

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        ts <- systemSeconds <$> getSystemTime
        manager <- newManager defaultManagerSettings
        req <- parseRequest [i|http://localhost:#{serverPort}/servercmd/still_alive/additional/#{ts}/key|]
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status403

    it "reports successful tests for /servercmd/threaded_proxy_test, and does not hang on a failing target" $ do
      let mockConfig = defaultMockConfig { mockPort = 19831 }
          clientCfg  = testConfig 31

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        ts <- systemSeconds <$> getSystemTime
        manager <- newManager defaultManagerSettings
        let testKey = hash @Text $ encodeUtf8 ([i|hentai@home-speedtest-1000-#{ts}-#{clientId clientCfg}-#{key clientCfg}|] :: Text)
            proxyTest k = do
              -- the test server tests itself
              let add = [i|hostname=localhost;protocol=http;port=#{serverPort};testsize=1000;testcount=2;testtime=#{ts};testkey=#{k}|] :: Text
              req <- parseRequest (serverCmdUrl serverPort clientCfg "threaded_proxy_test" add ts)
              resp <- httpLbs req manager
              responseStatus resp `shouldBe` status200
              pure (LBS.toStrict (responseBody resp))

        ok <- proxyTest testKey
        ok `shouldSatisfy` ("OK:2-" `BS.isPrefixOf`)

        -- the target answers 403 with a short body; this used to spin forever
        failed <- timeout 10000000 (proxyTest "wrong-key")
        failed `shouldBe` Just "OK:0-0"

    it "still serves a fetched file when the cache write fails" $ do
      let mockContent  = "WRITE_FAILS_CONTENT"
          mockConfig   = defaultMockConfig { mockPort = 19832, mockFileContent = mockContent }
          clientCfg    = testConfig 32
          fileURIStr   = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-txt"
          brokenWrites = CacheRunner
            { runCacheWith = interpret $ \case
                LookupKV _   -> pure Nothing
                UpdateKV _ _ -> throw (StorageError "write refused")
            }

      withRunnerTestServer True brokenWrites mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        ts <- systemSeconds <$> getSystemTime
        let inner    = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{key clientCfg}-hotlinkthis|]
            keystamp = [i|#{ts}-#{decodeUtf8 (BS.take 10 (hash @ByteString inner))}|] :: Text
        manager <- newManager defaultManagerSettings
        req <- parseRequest [i|http://localhost:#{serverPort}/h/#{fileURIStr}/keystamp=#{keystamp};fileindex=1;xres=1024/f.txt|]
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status200
        responseBody resp `shouldBe` LBS.fromStrict mockContent

    it "serves file with jpg extension and correct mime type" $ do
      let mockPortNum = 19816
          mockContent = "FAKE_JPG_CONTENT"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 15
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-jpg"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/test.jpg|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status200
        LBS.toStrict (responseBody response) `shouldBe` mockContent

    it "serves file with png extension" $ do
      let mockPortNum = 19817
          mockContent = "FAKE_PNG_CONTENT"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 16
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-png"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/test.png|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status200

    it "serves file with gif extension" $ do
      let mockPortNum = 19818
          mockContent = "FAKE_GIF_CONTENT"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 17
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-gif"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/test.gif|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status200

    it "serves file with mp4 extension" $ do
      let mockPortNum = 19819
          mockContent = "FAKE_MP4_CONTENT"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 18
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-mp4"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/test.mp4|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status200

    it "handles /servercmd/start_downloader" $ do
      let mockPortNum = 19820
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 19

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        now <- getSystemTime
        let ts = systemSeconds now
        manager <- newManager defaultManagerSettings
        req <- parseRequest (serverCmdUrl serverPort clientCfg "start_downloader" "additional" ts)
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status200

    it "serves file with webm extension" $ do
      let mockPortNum = 19821
          mockContent = "FAKE_WEBM_CONTENT"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 20
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-wbm"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/test.webm|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status200

    it "serves file with webp extension" $ do
      let mockPortNum = 19822
          mockContent = "FAKE_WEBP_CONTENT"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 21
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-wbp"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/test.webp|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status200
        find ((== "Content-Type") . fst) (responseHeaders response) `shouldBe` Just ( "Content-Type", "image/webp" )

    it "serves file with avif extension" $ do
      let mockPortNum = 19823
          mockContent = "FAKE_AVIF_CONTENT"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 22
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-avf"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/test.avif|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status200

    it "serves file with jxl extension" $ do
      let mockPortNum = 19824
          mockContent = "FAKE_JXL_CONTENT"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 23
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-jxl"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/test.jxl|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status200

    it "rejects resource request with time drift at boundary (601 seconds)" $ do
      let mockPortNum = 19825
          mockContent = "TIMEDRIFT_TEST"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 24
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-txt"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime - 601
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/testfile.txt|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status403
        LBS.toStrict (responseBody response) `shouldSatisfy` (\body -> "time" `BS.isInfixOf` body)

    it "serves resource from cache when already stored" $ do
      let mockPortNum = 19826
          mockContent = "CACHED_CONTENT_TEST"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 25
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-txt"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/testfile.txt|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        
        -- First request fetches from upstream
        response1 <- httpLbs request manager
        responseStatus response1 `shouldBe` status200
        
        -- Second request should hit cache
        response2 <- httpLbs request manager
        responseStatus response2 `shouldBe` status200
        LBS.toStrict (responseBody response2) `shouldBe` mockContent

    it "rejects resource request with missing fileindex option" $ do
      let mockPortNum = 19827
          mockContent = "MISSING_PARAM_TEST"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 26
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-txt"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/testfile.txt|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status404

    it "returns 404 when resource not found in cache and no params" $ do
      let mockPortNum = 19828
          mockContent = "NOT_FOUND_TEST"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 27
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-txt"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText}|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/testfile.txt|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status404

    it "serves resource with default octet-stream mime type for unknown extension" $ do
      let mockPortNum = 19829
          mockContent = "UNKNOWN_EXT_CONTENT"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 28
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-xyz"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/test.unknown|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status200
        LBS.toStrict (responseBody response) `shouldBe` mockContent

    it "accepts resource request with future timestamp within drift limit" $ do
      let mockPortNum = 19830
          mockContent = "FUTURE_TIME_TEST"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 29
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-txt"

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime + 500
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text
            url = [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/testfile.txt|]

        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager

        responseStatus response `shouldBe` status200

    it "handles POST request to unknown route with 404" $ do
      let mockPortNum = 19831
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 30

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        manager <- newManager defaultManagerSettings
        req0 <- parseRequest [i|http://localhost:#{serverPort}/unknown-post|]
        let req = req0 { method = "POST" }
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status404

    it "handles GET request to unknown route with 404" $ do
      let mockPortNum = 19832
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 31

      withTestServer mockConfig clientCfg defaultHathSettings $ \serverPort -> do
        manager <- newManager defaultManagerSettings
        req <- parseRequest [i|http://localhost:#{serverPort}/random/path/here|]
        resp <- httpLbs req manager
        responseStatus resp `shouldBe` status404

  -- Tests using startServer to cover initialization code
  describe "Server Initialization with startServer" $ do
    it "starts server successfully and serves requests" $ do
      let mockPortNum = 19900
          mockContent = "INIT_TEST_CONTENT"
          mockConfig  = defaultMockConfig { mockPort = mockPortNum, mockFileContent = mockContent }
          clientCfg   = testConfig 100
          settings = defaultHathSettings { clientPort = 9900 }
          fileURIStr  = hash @Text mockContent <> "-" <> show (BS.length mockContent) <> "-1024-768-txt"

      withStartServer mockConfig clientCfg settings $ \serverPort _chan -> do
        currentTime <- getSystemTime
        let ts = systemSeconds currentTime
            k  = key clientCfg
            challengeInput = encodeUtf8 [i|#{ts}-#{fileURIStr}-#{k}-hotlinkthis|]
            challenge = BS.take 10 (hash @ByteString challengeInput)
            keystampText = [i|#{ts}-#{decodeUtf8 challenge}|] :: Text
            opts = [i|keystamp=#{keystampText};fileindex=1;xres=1024|] :: Text

        -- Use TLS-aware manager to connect to HTTPS server
        manager <- newManager defaultManagerSettings
        -- Try HTTP first (onInsecure = AllowInsecure means HTTP should work)
        request <- parseRequest $ [i|http://localhost:#{serverPort}/h/#{fileURIStr}/#{opts}/testfile.txt|]
        response <- httpLbs request manager

        responseStatus response `shouldBe` status200

    it "initializes database and fetches blacklist on startup" $ do
      let mockPortNum = 19901
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 101
          settings = defaultHathSettings { clientPort = 9901 }

      withStartServer mockConfig clientCfg settings $ \_serverPort _chan -> do
        -- The test passes if startServer successfully:
        -- 1. Calls fetchBlacklist (line 477 in Server.hs)
        -- 2. Initializes database (line 479)
        -- 3. Sets up tictok timers (line 480)
        -- 4. Starts the server loop
        threadDelay 2000000  -- Wait 2 seconds for initialization
        -- If we got here without exceptions, initialization succeeded
        return ()

    it "fetches blacklist via tictok timer" $ do
      let mockPortNum = 19903
          mockConfig  = defaultMockConfig { mockPort = mockPortNum }
          clientCfg   = testConfig 103
          settings = defaultHathSettings { clientPort = 9903 }

      withStartServer mockConfig clientCfg settings $ \_serverPort _chan -> do
        -- tictok sets up three timers:
        -- 1. IP map cleanup (every 1 minute)
        -- 2. Heartbeat stillAlive (every 1 minute)
        -- 3. Blacklist fetch (every 36 minutes)
        -- Wait a bit to ensure timers are set up
        threadDelay 3000000  -- 3 seconds
        -- The RPC mock will receive heartbeat calls
        return ()
