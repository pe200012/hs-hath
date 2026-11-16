-- | The client end to end: the real application, and at the end the real TLS
-- server, against a mock RPC server.
module Integration ( integrationSpecs ) where

import           Control.Concurrent        ( forkIO, killThread, threadDelay )
import           Control.Exception         ( IOException, bracket, catch, throwIO, try )

import qualified Data.ByteString           as BS
import qualified Data.List                 as List
import           Data.String.Interpolate   ( i )
import qualified Data.Text                 as T
import           Data.Time.Clock.System    ( SystemTime(systemSeconds), getSystemTime )
import           Data.X509                 ( CertificateChain(..) )
import           Data.X509.File            ( readKeyFile, readSignedObject )

import           Database.SQLite.Simple    ( Connection, SQLError, close, open )

import           Hath.Cache                ( Cache(..), CacheRunner(..) )
import           Hath.Cache.Filesystem     ( newFsCache, runCacheFilesystem )
import           Hath.Cache.SQLite         ( initializeDB, runCacheSQLite )
import           Hath.Config               ( ClientConfig(..), defaultClientConfig )
import           Hath.Error                ( HathError(StorageError) )
import           Hath.Protocol             ( sha1HexText )
import           Hath.Server               ( ServerAction(..)
                                           , ServerEnv(..)
                                           , ServerOptions(..)
                                           , makeApplication
                                           , startServer
                                           )
import           Hath.Server.FloodControl  ( newFloodControl )
import           Hath.Settings             ( HathSettings(..), defaultHathSettings, parseSettings )
import           Hath.Stats                ( newStatsEnv )

import           MockRPC

import           Network.HTTP.Client       ( HttpException
                                           , Response
                                           , defaultManagerSettings
                                           , httpLbs
                                           , method
                                           , newManager
                                           , parseRequest
                                           , redirectCount
                                           , responseBody
                                           , responseHeaders
                                           , responseStatus
                                           )
import           Network.HTTP.Types        ( HeaderName
                                           , status200
                                           , status301
                                           , status400
                                           , status403
                                           , status404
                                           , status502
                                           , statusCode
                                           )
import           Network.Socket            ( tupleToHostAddress )
import qualified Network.Socket            as Sock
import           Network.Socket.ByteString ( recv, sendAll )
import qualified Network.TLS               as TLS
import qualified Network.Wai.Handler.Warp  as Warp

import           Polysemy                  ( interpret )
import           Polysemy.Error            ( throw )

import           Relude

import           System.Directory          ( createDirectoryIfMissing )
import           System.FilePath           ( (</>) )
import           System.IO.Temp            ( withSystemTempDirectory )
import           System.Process            ( readProcess )
import           System.Timeout            ( timeout )

import           Test.Hspec

import           UnliftIO                  ( TChan, newTChanIO, writeTChan )

testConfig :: ClientConfig
testConfig
  = defaultClientConfig { clientId    = "test-client-001"
                        , key         = "test-secret-key-12345"
                        , downloadDir = "/tmp/hs-hath-test"
                        }

-- * The application under test

-- | Run the application on a free port, with the settings the mock RPC server
-- assigns at login.
withAppUsing :: Bool -> CacheRunner -> MockRPC -> (Int -> IO a) -> IO a
withAppUsing floodControlOn cache mock action = withMockRPC mock $ do
  actions <- newTChanIO
  downloadRequest <- newEmptyMVar
  settings <- newTVarIO (parseSettings defaultHathSettings (loginSettings mock))
  stats <- newStatsEnv
  floodControl <- if floodControlOn
    then Just <$> newFloodControl
    else pure Nothing
  let app
        = makeApplication
          ServerEnv { envConfig = testConfig
                    , envSettings = settings
                    , envActions = actions
                    , envDownloadRequest = downloadRequest
                    , envStats = stats
                    , envCache = cache
                    , envFloodControl = floodControl
                    , envTrustProxyHeaders = False
                    }
  Warp.testWithApplication (pure app) action

-- | The application over an empty in-memory SQLite cache, without flood control.
withMock :: MockRPC -> (Int -> IO a) -> IO a
withMock mock action = withMemoryCache $ \cache -> withAppUsing False cache mock action

-- | The same, with an upstream that serves @content@ for any file.
withApp :: ByteString -> (Int -> IO a) -> IO a
withApp content = withMock defaultMockRPC { mockFileContent = content }

-- | The application over a Filesystem cache that holds a file with this
-- content, given its id. Upstream would send something else, so a match proves
-- the bytes came from disk.
withFilesystemHit :: ByteString -> (Int -> Text -> IO a) -> IO a
withFilesystemHit content action = withSystemTempDirectory "hath-fs" $ \root -> do
  let fileId    = fileIdFor content "jpg"
      directory = root </> toString (T.take 4 fileId)
  createDirectoryIfMissing True directory
  writeFileBS (directory </> toString fileId) content
  fs <- newFsCache root
  withAppUsing
    False
    CacheRunner { runCacheWith = runCacheFilesystem fs }
    defaultMockRPC { mockFileContent = "UPSTREAM_CONTENT" }
    (`action` fileId)

withMemoryCache :: (CacheRunner -> IO a) -> IO a
withMemoryCache use = bracket (open ":memory:") closeWhenIdle $ \conn -> do
  initializeDB conn
  use CacheRunner { runCacheWith = runCacheSQLite conn }

-- | A cache miss is written back after the response has gone out, so a test can
-- finish while that write still holds the database; wait for it (up to a second).
closeWhenIdle :: Connection -> IO ()
closeWhenIdle conn = go (100 :: Int)
  where
    go n = close conn `catch` \(e :: SQLError) -> if n > 0
      then threadDelay 10000 >> go (n - 1)
      else throwIO e

-- * Requests, signed the way the H\@H server signs them

now :: IO Int64
now = systemSeconds <$> getSystemTime

signedWithKey :: Text -> Text
signedWithKey = sha1HexText . encodeUtf8

-- | The id the server gives a file with this content and type.
fileIdFor :: ByteString -> Text -> Text
fileIdFor content ext
  = sha1HexText content <> "-" <> show (BS.length content) <> "-1024-768-" <> ext

-- | The path a reader asks for a file at, with a keystamp issued at @time@.
-- @options@ are appended to the keystamp, e.g. 'fetchable'.
filePath :: Text -> Int64 -> Text -> String
filePath fileId time options = [i|/h/#{fileId}/keystamp=#{time}-#{stamp}#{options}/file|]
  where
    stamp = T.take 10 $ signedWithKey [i|#{time}-#{fileId}-#{key testConfig}-hotlinkthis|]

-- | The options that let a client fetch a file it does not have.
fetchable :: Text
fetchable = ";fileindex=1;xres=1024"

-- | The path of a server command sent at @time@.
commandPath :: Text -> Text -> Int64 -> String
commandPath command additional time
  = [i|/servercmd/#{command}/#{additional}/#{time}/#{commandKey}|]
  where
    commandKey
      = signedWithKey
        [i|hentai@home-servercmd-#{command}-#{additional}-#{clientId testConfig}-#{time}-#{key testConfig}|]

-- | The key of a speed test of that size at @time@.
speedTestKeyFor :: Int -> Int64 -> Text
speedTestKeyFor size time
  = signedWithKey
    [i|hentai@home-speedtest-#{size}-#{time}-#{clientId testConfig}-#{key testConfig}|]

request :: ByteString -> Int -> String -> IO (Response LByteString)
request verb port path = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest [i|http://localhost:#{port}#{path}|]
  httpLbs req { method = verb, redirectCount = 0 } manager

fetch :: Int -> String -> IO (Response LByteString)
fetch = request "GET"

body :: Response LByteString -> ByteString
body = toStrict . responseBody

header :: HeaderName -> Response LByteString -> Maybe ByteString
header name = List.lookup name . responseHeaders

-- * Specs

integrationSpecs :: Spec
integrationSpecs = do
  describe "Files" $ do
    it "fetches a file it does not have and serves it" $ do
      let content = "TEST_FILE_CONTENT_123"
      withApp content $ \port -> do
        time <- now
        response <- fetch port (filePath (fileIdFor content "jpg") time fetchable)
        responseStatus response `shouldBe` status200
        body response `shouldBe` content

    it "serves it again once it is cached" $ do
      let content = "CACHED_CONTENT_TEST"
      withApp content $ \port -> do
        time <- now
        let path = filePath (fileIdFor content "jpg") time fetchable
        fetched <- fetch port path
        responseStatus fetched `shouldBe` status200
        cached <- fetch port path
        responseStatus cached `shouldBe` status200
        body cached `shouldBe` content

    forM_
      [ ( "jpg", "image/jpeg" )
      , ( "png", "image/png" )
      , ( "gif", "image/gif" )
      , ( "mp4", "video/mp4" )
      , ( "wbm", "video/webm" )
      , ( "wbp", "image/webp" )
      , ( "avf", "image/avif" )
      , ( "jxl", "image/jxl" )
      ]
      $ \( ext, contentType ) -> it [i|serves a .#{ext} file as #{contentType}|] $ do
        let content = "CONTENT_OF_A_FILE"
        withApp content $ \port -> do
          time <- now
          response <- fetch port (filePath (fileIdFor content ext) time fetchable)
          responseStatus response `shouldBe` status200
          body response `shouldBe` content
          header "Content-Type" response `shouldBe` Just contentType

    it "sends the official client's headers, and no User-Agent" $ withApp "x" $ \port -> do
      time <- now
      response <- fetch port (filePath (fileIdFor "x" "jpg") time fetchable)
      map (`header` response) [ "Server", "Connection", "Cache-Control", "Content-Length" ]
        `shouldBe` map
          Just
          [ "Genetic Lifeform and Distributed Open Server 1.6.6"
          , "close"
          , "public, max-age=31536000"
          , "1"
          ]
      map (`header` response) [ "User-Agent", "X-Content-Type-Options" ]
        `shouldBe` [ Nothing, Nothing ]

    it "serves a Filesystem cache hit from disk"
      $ withFilesystemHit "ON_DISK_CONTENT"
      $ \port fileId -> do
        time <- now
        response <- fetch port (filePath fileId time fetchable)
        responseStatus response `shouldBe` status200
        body response `shouldBe` "ON_DISK_CONTENT"
        header "Content-Length" response `shouldBe` Just "15"
        header "Content-Type" response `shouldBe` Just "image/jpeg"

    it "does not serve even a cached file without its fileindex and xres, as the official client"
      $ withFilesystemHit "ON_DISK_CONTENT"
      $ \port fileId -> do
        time <- now
        response <- fetch port (filePath fileId time "")
        responseStatus response `shouldBe` status404

    it "serves a file id written with leading zeros" $ do
      let content = "LEADING_ZEROS"
          fileId  = sha1HexText content <> "-0013-01024-768-jpg"
      withApp content $ \port -> do
        time <- now
        response <- fetch port (filePath fileId time fetchable)
        responseStatus response `shouldBe` status200
        body response `shouldBe` content

    it "still serves a fetched file when the cache write fails" $ do
      let content      = "WRITE_FAILS_CONTENT"
          brokenWrites = CacheRunner { runCacheWith = interpret $ \case
                                         LookupFile _  -> pure Nothing
                                         StoreFile _ _ -> throw (StorageError "write refused")
                                         DeleteFile _  -> pass }
      withAppUsing False brokenWrites defaultMockRPC { mockFileContent = content } $ \port -> do
        time <- now
        response <- fetch port (filePath (fileIdFor content "jpg") time fetchable)
        responseStatus response `shouldBe` status200
        body response `shouldBe` content

    it "refuses an invalid keystamp" $ withApp "x" $ \port -> do
      response <- fetch port [i|/h/#{fileIdFor "x" "jpg"}/keystamp=invalid#{fetchable}/file|]
      responseStatus response `shouldBe` status403

    it "refuses a keystamp 900 seconds old, with the official error page" $ withApp "x" $ \port
      -> do
        time <- now
        response <- fetch port (filePath (fileIdFor "x" "jpg") (time - 900) fetchable)
        responseStatus response `shouldBe` status403
        body response `shouldBe` "An error has occurred. (403)"
        header "Content-Type" response `shouldBe` Just "text/html; charset=ISO-8859-1"

    -- a few seconds short of 900, for the time the request takes
    forM_ [ -890, 890 ]
      $ \drift -> it [i|accepts a keystamp #{drift} seconds from now|] $ withApp "x" $ \port -> do
        time <- now
        response <- fetch port (filePath (fileIdFor "x" "jpg") (time + drift) fetchable)
        responseStatus response `shouldBe` status200

    it "does not find a file it lacks when the request has no fileindex" $ withApp "x" $ \port -> do
      time <- now
      response <- fetch port (filePath (fileIdFor "x" "jpg") time ";xres=1024")
      responseStatus response `shouldBe` status404

    it "does not find a file it lacks when the request has no options" $ withApp "x" $ \port -> do
      time <- now
      response <- fetch port (filePath (fileIdFor "x" "jpg") time "")
      responseStatus response `shouldBe` status404

    it "does not ask the RPC server about a file id the official client refuses" $ do
      seen <- newIORef []
      withMock defaultMockRPC { mockSeen = atomicModifyIORef'_ seen . (:) . fst } $ \port -> do
        time <- now
        response <- fetch port (filePath (fileIdFor "x" "txt") time fetchable)
        responseStatus response `shouldBe` status404
      readIORef seen `shouldReturn` []

    it "does not find a file when srfetch fails"
      $ withMock defaultMockRPC { mockOutages = 3 }
      $ \port -> do
        time <- now
        response <- fetch port (filePath (fileIdFor "x" "jpg") time fetchable)
        responseStatus response `shouldBe` status404

    it "answers 502 when the source does not send the file" $ withApp "NOT_THE_FILE" $ \port -> do
      time <- now
      response <- fetch port (filePath (fileIdFor "x" "jpg") time fetchable)
      responseStatus response `shouldBe` status502
      body response `shouldBe` "An error has occurred. (502)"

    it "goes on fetching a file whose reader hung up, and caches it" $ do
      let content = encodeUtf8 (T.replicate 200000 "z")
      seen <- newIORef []
      withMock
        defaultMockRPC { mockFileContent  = content
                       , mockPauseHalfway = 500000
                       , mockSeen         = atomicModifyIORef'_ seen . (:) . fst
                       }
        $ \port -> do
          time <- now
          let path = filePath (fileIdFor content "jpg") time fetchable
          bracket (connectLocal port) Sock.close $ \sock -> do
            sendAll sock $ "GET " <> encodeUtf8 path <> " HTTP/1.1\r\nHost: localhost\r\n\r\n"
            -- the start of the reply, then hang up
            void $ recv sock 4096
          -- the pause, and time to reach the cache
          threadDelay 2000000
          body <$> fetch port path `shouldReturn` content
      readIORef seen `shouldReturn` [ "srfetch" ]

    it "drops the reader's connection when upstream sends less than it announced" $ do
      let content = encodeUtf8 (T.replicate 100000 "y")
      seen <- newIORef []
      withMock
        defaultMockRPC { mockFileContent   = content
                       , mockHangUpHalfway = True
                       , mockSeen          = atomicModifyIORef'_ seen . (:) . fst
                       }
        $ \port -> do
          time <- now
          let path = filePath (fileIdFor content "jpg") time fetchable
          -- http-client fails a body that ends short when the connection
          -- closes; left open, it would wait for the rest
          cutShort <- timeout 10000000 $ try @HttpException (fetch port path)
          cutShort `shouldSatisfy` maybe False isLeft
          -- nothing was cached: the next request asks for the file again
          void $ timeout 10000000 $ try @HttpException (fetch port path)
      readIORef seen `shouldReturn` [ "srfetch", "srfetch" ]

    it "exempts local addresses from flood control" $ withMemoryCache $ \cache
      -> withAppUsing True cache defaultMockRPC { mockFileContent = "x" } $ \port -> do
        time <- now
        responses <- replicateM 13 $ fetch port (filePath (fileIdFor "x" "jpg") time fetchable)
        responseStatus <$> responses `shouldBe` replicate 13 status200

  describe "Server commands" $ do
    it "answers still_alive" $ withApp "x" $ \port -> do
      time <- now
      response <- fetch port (commandPath "still_alive" "additional" time)
      responseStatus response `shouldBe` status200
      body response `shouldBe` "I feel FANTASTIC and I'm still alive"
      header "Content-Type" response `shouldBe` Just "text/html; charset=ISO-8859-1"

    it "takes a command name in any case" $ withApp "x" $ \port -> do
      time <- now
      response <- fetch port (commandPath "Still_Alive" "" time)
      body response `shouldSatisfy` ("FANTASTIC" `BS.isInfixOf`)

    forM_
      [ ( "speed_test", "testsize=1000" )
      , ( "refresh_settings", "additional" )
      , ( "refresh_certs", "additional" )
      , ( "start_downloader", "additional" )
      ]
      $ \( command, additional ) -> it [i|accepts #{command}|] $ withApp "x" $ \port -> do
        time <- now
        response <- fetch port (commandPath command additional time)
        responseStatus response `shouldBe` status200

    it "answers refresh_settings with 200 and nothing when the refresh fails"
      $ withMock defaultMockRPC { mockOutages = 3 }
      $ \port -> do
        time <- now
        response <- fetch port (commandPath "refresh_settings" "" time)
        responseStatus response `shouldBe` status200
        body response `shouldBe` ""

    forM_
      [ ( "an unknown command", "stand_up", "" )
      , ( "threaded_proxy_test without its parameters", "threaded_proxy_test", "additional" )
      , ( "speed_test with a testsize that is not a number", "speed_test", "testsize=lots" )
      ]
      $ \( what :: Text, command, additional )
      -> it [i|answers #{what} with INVALID_COMMAND|] $ withApp "x" $ \port -> do
        time <- now
        response <- fetch port (commandPath command additional time)
        responseStatus response `shouldBe` status200
        body response `shouldBe` "INVALID_COMMAND"

    it "refuses a command without a valid key" $ withApp "x" $ \port -> do
      time <- now
      response <- fetch port [i|/servercmd/still_alive/additional/#{time}/key|]
      responseStatus response `shouldBe` status403

    it "refuses a command with a part missing" $ withApp "x" $ \port -> do
      time <- now
      response <- fetch port [i|/servercmd/still_alive/additional/#{time}|]
      responseStatus response `shouldBe` status403

    it "refuses a command from an address rpc_server_ip does not list"
      $ withMock defaultMockRPC { mockRPCServerIP = "192.0.2.1" }
      $ \port -> do
        time <- now
        response <- fetch port (commandPath "still_alive" "" time)
        responseStatus response `shouldBe` status403

    it "reports the successful tests of threaded_proxy_test, and does not hang on a failing target"
      $ withApp "x"
      $ \port -> do
        time <- now
        let proxyTest testKey = do
              -- the server tests itself
              let additional
                    = [i|hostname=localhost;protocol=http;port=#{port};testsize=1000;testcount=2;testtime=#{time};testkey=#{testKey}|]
              response <- fetch port (commandPath "threaded_proxy_test" additional time)
              responseStatus response `shouldBe` status200
              pure (body response)
        ok <- proxyTest (speedTestKeyFor 1000 time)
        ok `shouldSatisfy` ("OK:2-" `BS.isPrefixOf`)
        -- the target answers 403 with a short body; this used to spin forever
        failed <- timeout 10000000 (proxyTest "wrong-key")
        failed `shouldBe` Just "OK:0-0"

  describe "Speed tests" $ do
    forM_ [ "/nothing", "" ] $ \(rest :: Text)
      -> it [i|sends the bytes asked for with a valid key (#{rest})|] $ withApp "x" $ \port -> do
        time <- now
        response <- fetch port [i|/t/1000/#{time}/#{speedTestKeyFor 1000 time}#{rest}|]
        responseStatus response `shouldBe` status200
        BS.length (body response) `shouldBe` 1000

    it "refuses a test from another time" $ withApp "x" $ \port -> do
      response <- fetch port "/t/1000/1000000/invalid-key/nothing"
      responseStatus response `shouldBe` status403

    it "refuses a test 301 seconds old" $ withApp "x" $ \port -> do
      time <- subtract 301 <$> now
      response <- fetch port [i|/t/1000/#{time}/#{speedTestKeyFor 1000 time}|]
      responseStatus response `shouldBe` status403

    it "does not understand a test with a part missing" $ withApp "x" $ \port -> do
      time <- now
      response <- fetch port [i|/t/1000/#{time}|]
      responseStatus response `shouldBe` status400

  describe "Other routes" $ do
    it "reports metrics" $ withApp "x" $ \port -> do
      response <- fetch port "/metrics"
      responseStatus response `shouldBe` status200
      body response `shouldSatisfy` ("hath_cache_sent_total" `BS.isInfixOf`)
      -- a scrape must not be cached like a file
      header "Cache-Control" response `shouldBe` Nothing

    it "disallows robots" $ withApp "x" $ \port -> do
      response <- fetch port "/robots.txt"
      responseStatus response `shouldBe` status200
      body response `shouldBe` "User-agent: *\nDisallow: /"
      header "Content-Type" response `shouldBe` Just "text/plain; charset=ISO-8859-1"

    it "redirects /favicon.ico to e-hentai.org" $ withApp "x" $ \port -> do
      response <- fetch port "/favicon.ico"
      responseStatus response `shouldBe` status301
      header "Location" response `shouldBe` Just "https://e-hentai.org/favicon.ico"

    forM_
      [ ( "GET", "/unknown-path", status404 )
      , ( "GET", "/random/path/here", status404 )
      , ( "GET", "/h/only-an-id", status400 )
      , ( "POST", "/unknown-post", status400 )
      , ( "PUT", "/robots.txt", status400 )
      ]
      $ \( verb, path, status )
      -> it [i|answers #{verb} #{path} with #{statusCode status}|] $ withApp "x" $ \port -> do
        response <- request verb port path
        responseStatus response `shouldBe` status
        body response `shouldBe` [i|An error has occurred. (#{statusCode status})|]

  describe "HEAD" $ do
    it "is answered as GET, without the body" $ withApp "CONTENT" $ \port -> do
      time <- now
      let path = filePath (fileIdFor "CONTENT" "png") time fetchable
      response <- request "HEAD" port path
      responseStatus response `shouldBe` status200
      map (`header` response) [ "Content-Type", "Content-Length" ]
        `shouldBe` [ Just "image/png", Just "7" ]
      body response `shouldBe` ""

    it "goes through the same checks" $ withApp "x" $ \port -> do
      time <- now
      refused <- request "HEAD" port (filePath (fileIdFor "x" "jpg") (time - 900) fetchable)
      responseStatus refused `shouldBe` status403
      unknown <- request "HEAD" port "/anything"
      responseStatus unknown `shouldBe` status404
      header "Content-Length" unknown `shouldBe` Just "28"
      body unknown `shouldBe` ""

    it "of a file it lacks fetches it, as the official client" $ do
      seen <- newIORef []
      withMock
        defaultMockRPC { mockFileContent = "x", mockSeen = atomicModifyIORef'_ seen . (:) . fst }
        $ \port -> do
          time <- now
          let path = filePath (fileIdFor "x" "jpg") time fetchable
          responseStatus <$> request "HEAD" port path `shouldReturn` status200
          -- the fetch finishes on its own; give it a moment to reach the cache
          threadDelay 1000000
          body <$> fetch port path `shouldReturn` "x"
      readIORef seen `shouldReturn` [ "srfetch" ]

  describe "The TLS server" $ do
    it "starts, and serves a file" $ do
      let content = "INIT_TEST_CONTENT"
      withServer (const defaultMockRPC { mockFileContent = content }) $ \port _ -> do
        time <- now
        -- plain HTTP is allowed on the TLS port
        response <- fetch port (filePath (fileIdFor content "jpg") time fetchable)
        responseStatus response `shouldBe` status200
        body response `shouldBe` content

    it "swaps certificates in place: open connections survive, new handshakes get the new one"
      $ withSystemTempDirectory "hath-cert"
      $ \dir -> do
        let openssl = void . flip (readProcess "openssl") ""
        -- the certificate the mock RPC server hands out on refresh; with one day
        -- left it would be refused
        openssl
          [ "req"
          , "-x509"
          , "-newkey"
          , "rsa:2048"
          , "-nodes"
          , "-days"
          , "2"
          , "-subj"
          , "/CN=rotated"
          , "-keyout"
          , dir </> "key.pem"
          , "-out"
          , dir </> "cert.pem"
          ]
        openssl
          [ "pkcs12"
          , "-export"
          , "-inkey"
          , dir </> "key.pem"
          , "-in"
          , dir </> "cert.pem"
          , "-out"
          , dir </> "cert.p12"
          , "-passout"
          , "pass:" <> toString (key testConfig)
          ]
        p12 <- readFileBS (dir </> "cert.p12")
        rotated <- CertificateChain <$> readSignedObject (dir </> "cert.pem")
        withServer (const defaultMockRPC { mockCertificate = p12 }) $ \port actions -> do
          servedCertificate port >>= (`shouldNotBe` Just rotated)
          bracket (connectLocal port) Sock.close $ \sock -> do
            -- a request still being sent while the certificate changes
            sendAll sock "GET /robots.txt HTTP/1.1\r\nHost: localhost\r\n"
            atomically $ writeTChan actions Cert
            let waitForRotated = do
                  served <- servedCertificate port
                  unless (served == Just rotated) $ threadDelay 100000 >> waitForRotated
            timeout 5000000 waitForRotated >>= (`shouldBe` Just ())
            sendAll sock "\r\n"
            reply <- recv sock 4096
            reply `shouldSatisfy` ("HTTP/1.1 200" `BS.isPrefixOf`)

    it "listens before it sends client_start" $ do
      listeningAtStart <- newEmptyMVar
      let mock port = defaultMockRPC { mockSeen = \( act, _ ) -> when (act == "client_start")
                                         $ void . tryPutMVar listeningAtStart =<< accepts port }
      withServer mock $ \_ _ -> takeMVar listeningAtStart >>= (`shouldBe` True)

    it "on shutdown, sends client_stop once, serves 5 s more, then stops listening" $ do
      seen <- newIORef []
      let mock _ = defaultMockRPC { mockSeen = atomicModifyIORef'_ seen . (:) . fst }
      withServer mock $ \port actions -> do
        atomically $ writeTChan actions GracefulShutdown
        threadDelay 1000000
        readIORef seen >>= (`shouldBe` [ "client_stop" ]) . filter (== "client_stop")
        response <- fetch port "/robots.txt"
        responseStatus response `shouldBe` status200
        threadDelay 6000000
        accepts port >>= (`shouldBe` False)

-- * The real server

-- | Run 'startServer' on a free port with an SQLite cache in a temporary
-- directory, and shut it down afterwards. The mock RPC server is told the port.
withServer :: (Int -> MockRPC) -> (Int -> TChan ServerAction -> IO a) -> IO a
withServer mock action = withSystemTempDirectory "hath-server" $ \dir -> do
  port <- bracket Warp.openFreePort (Sock.close . snd) (pure . fst)
  withMockRPC (mock port) $ do
    -- warp-tls' own test pair: its age does not matter here
    certs <- CertificateChain <$> readSignedObject "vendor/warp-tls-3.4.13/certificate.pem"
    privateKey <- readKeyFile "vendor/warp-tls-3.4.13/key.pem"
      >>= maybe (fail "no private key in the test key file") pure . listToMaybe
    actions <- newTChanIO
    let config   = testConfig { cachePath = toText (dir </> "cache.db") }
        settings = defaultHathSettings { clientPort = port }
        options  = ServerOptions { disableRateLimit = True, trustProxyHeaders = False }
    bracket (forkIO $ startServer config options settings ( certs, privateKey ) actions) killThread
      $ \_ -> do
        waitUntilListening port
        result <- action port actions
        atomically $ writeTChan actions GracefulShutdown
        -- time to tell the RPC server before the mock goes away
        threadDelay 500000
        pure result

-- | Whether something accepts connections on the port.
accepts :: Int -> IO Bool
accepts port = isRight <$> try @IOException (bracket (connectLocal port) Sock.close (const pass))

-- | Wait, up to ten seconds, until something accepts connections on the port.
waitUntilListening :: Int -> IO ()
waitUntilListening port = go (100 :: Int)
  where
    go n = try @IOException (bracket (connectLocal port) Sock.close (const pass)) >>= \case
      Right () -> pass
      Left e
        | n > 0 -> threadDelay 100000 >> go (n - 1)
        | otherwise -> throwIO e

connectLocal :: Int -> IO Sock.Socket
connectLocal port = do
  sock <- Sock.socket Sock.AF_INET Sock.Stream Sock.defaultProtocol
  Sock.connect sock (Sock.SockAddrInet (fromIntegral port) (tupleToHostAddress ( 127, 0, 0, 1 )))
    `catch` \(e :: IOException) -> Sock.close sock >> throwIO e
  pure sock

-- | The chain a TLS handshake presents, without SNI (as when connecting by IP).
servedCertificate :: Int -> IO (Maybe CertificateChain)
servedCertificate port = bracket (connectLocal port) Sock.close $ \sock -> do
  seen <- newIORef Nothing
  let params = TLS.defaultParamsClient "localhost" ""
  ctx <- TLS.contextNew
    sock
    params { TLS.clientUseServerNameIndication = False
           , TLS.clientHooks = (TLS.clientHooks params)
               { TLS.onServerCertificate = \_ _ _ chain -> [] <$ writeIORef seen (Just chain) }
           }
  TLS.handshake ctx
  void $ try @SomeException (TLS.bye ctx)
  readIORef seen
