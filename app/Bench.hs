{-# LANGUAGE BangPatterns #-}

-- | hath-bench: plays the H@H RPC server for a client running on this machine
-- (hs-hath or the official Java client), replays the measurements the real
-- server makes, and prints a scorecard.
--
-- The real server's quality inputs we can reproduce:
--   * servercmd speed_test / still_alive right after client_start
--   * servercmd threaded_proxy_test (the client measures someone else)
--   * other clients pulling /t/ (5 x 5 MB concurrently)
--   * readers pulling /h/ files, cold (srfetch + upstream) and warm
module Bench ( main ) where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Builder  as BB
import qualified Data.ByteString.Char8    as BSC
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.List                as List
import qualified Data.Map.Strict          as Map
import           Data.String.Interpolate  ( i )
import qualified Data.Text                as T
import           Data.Time.Clock.System   ( getSystemTime, systemSeconds )

import           GHC.Clock                ( getMonotonicTime )

import           Network.Connection       ( TLSSettings(TLSSettingsSimple) )
import qualified Network.HTTP.Client      as HC
import           Network.HTTP.Client.TLS  ( mkManagerSettings )
import qualified Network.HTTP.Types       as H
import           Network.TLS              ( defaultSupported )
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp

import           Options.Applicative

import           Relude

import           System.Directory         ( createDirectoryIfMissing, makeAbsolute )
import           System.Environment       ( getEnvironment )
import           System.FilePath          ( (</>) )
import           System.Process           ( CreateProcess(env), proc, readCreateProcess )

import           Text.Printf              ( printf )

import           Control.Concurrent       ( forkIO, threadDelay )

import           Control.Exception        ( try )

import           UnliftIO                 ( pooledMapConcurrentlyN, timeout )

import           Utils                    ( hash )

data Opts
  = Opts
  { oClientId   :: !Text
  , oClientKey  :: !Text
  , oRpcPort    :: !Int
  , oClientPort :: !Int
  , oFiles      :: !Int
  , oFileSize   :: !Int
  , oUpDelayMs  :: !Int
  , oUpKBps     :: !Int
  , oWorkDir    :: !FilePath
  , oClientHost :: !Text -- ^ where the bench reaches the client
  , oPublicHost :: !Text -- ^ where the client reaches the bench (also its rpc_server_ip)
  }

optsP :: Parser Opts
optsP
  = Opts
  <$> strOption (long "client-id" <> value "12345" <> showDefault <> help "Client ID of the client under test")
  <*> strOption
    (long "client-key"
     <> value "benchbenchbenchbench"
     <> showDefault
     <> help "Client key (the Java client wants 20 alphanumerics)")
  <*> option auto
    (long "rpc-port"
     <> value 19800
     <> showDefault
     <> help "Port of the fake RPC server, image server and speed-test target")
  <*> option auto (long "client-port" <> value 18443 <> showDefault <> help "Port the client is told to listen on")
  <*> option auto (long "files" <> value 30 <> showDefault <> help "Gallery size for the /h/ phases")
  <*> option auto (long "file-size" <> value 300000 <> showDefault <> help "Bytes per gallery file")
  <*> option auto
    (long "upstream-delay-ms"
     <> value 300
     <> showDefault
     <> help "Latency the fake image server adds before its first byte")
  <*> option auto
    (long "upstream-kbps"
     <> value 1000
     <> showDefault
     <> help "Fake image server bandwidth in KB/s (0 = unlimited)")
  <*> strOption (long "work-dir" <> value "bench-work" <> showDefault <> help "Where certs and client configs go")
  <*> strOption (long "client-host" <> value "127.0.0.1" <> showDefault <> help "Address of the client under test")
  <*> strOption
    (long "public-host"
     <> value "127.0.0.1"
     <> showDefault
     <> help "Address the client uses to reach this bench; anything but 127.0.0.1 listens on all IPv4 interfaces")

data GFile = GFile { gIndex :: !Int, gId :: !Text, gBytes :: !ByteString }

data Bench
  = Bench
  { bOpts     :: !Opts
  , bP12      :: !ByteString
  , bFiles    :: !(Map Text GFile)
  , bRpcCalls :: !(IORef (Map Text Int))
  , bSrfetch  :: !(IORef Int)
  , bStarted  :: !(MVar ())
  }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  o <- execParser
    (info (optsP <**> helper) (fullDesc <> progDesc "Fake H@H RPC server that measures a local client"))
  when (oFileSize o < 64) $ die "--file-size must be at least 64"
  wd <- makeAbsolute (oWorkDir o)
  createDirectoryIfMissing True (wd </> "java" </> "data")
  p12 <- makeCert o wd
  writeClientConfigs o wd
  -- a fresh nonce per run gives new file ids, so the cold phase is really cold
  nonce <- systemSeconds <$> getSystemTime
  let files = mkGallery nonce (oFiles o) (oFileSize o)
  b <- Bench o p12 (Map.fromList [ ( gId f, f ) | f <- files ])
    <$> newIORef mempty
    <*> newIORef 0
    <*> newEmptyMVar
  void
    $ forkIO
    $ Warp.runSettings
      (Warp.setHost (if oPublicHost o == "127.0.0.1" then "127.0.0.1" else "*4")
       $ Warp.setPort (oRpcPort o) Warp.defaultSettings)
      (app b)
  putTextLn (instructions o wd)
  takeMVar (bStarted b)
  -- the Java client only accepts normal connections once client_start has returned
  putTextLn "client_start received, starting in 3 s"
  threadDelay 3000000
  mgr <- HC.newManager
    (mkManagerSettings (TLSSettingsSimple True False False defaultSupported) Nothing)
    { HC.managerResponseTimeout = HC.responseTimeoutMicro (90 * 1000000) }
  rows <- runPhases b mgr files
  calls <- readIORef (bRpcCalls b)
  report o rows calls
  unless (all rowPassed rows) exitFailure

-- | Self-signed cert packed as PKCS#12 with the client key as password, which is what get_cert returns.
makeCert :: Opts -> FilePath -> IO ByteString
makeCert o wd = do
  environment <- getEnvironment
  let openssl args = void
        $ readCreateProcess
          (proc "openssl" args)
          { env = Just (( "HATH_BENCH_PASS", toString (oClientKey o) ) : environment) }
          ""
  openssl
    [ "req", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "7", "-subj", "/CN=127.0.0.1"
    , "-keyout", wd </> "key.pem", "-out", wd </> "cert.pem" ]
  openssl
    [ "pkcs12", "-export", "-inkey", wd </> "key.pem", "-in", wd </> "cert.pem"
    , "-out", wd </> "cert.p12", "-passout", "env:HATH_BENCH_PASS"
    , "-name", "hath.network" ] -- the alias the Java client looks up
  readFileBS (wd </> "cert.p12")

writeClientConfigs :: Opts -> FilePath -> IO ()
writeClientConfigs o wd = do
  writeFileText (wd </> "java" </> "data" </> "client_login") [i|#{oClientId o}-#{oClientKey o}|]
  writeFileText
    (wd </> "hs-hath.dhall")
    [i|let CacheBackend = < CacheBackendSQLite | CacheBackendR2 | CacheBackendFilesystem >
let ClientProxy = { host : Text, port : Integer, auth : Optional { _1 : Text, _2 : Text } }
in  { clientId = "#{oClientId o}"
    , key = "#{oClientKey o}"
    , version = "176"
    , proxy = None ClientProxy
    , downloadDir = "#{wd}/hs-download"
    , cachePath = "#{wd}/hs-cache.db"
    , cacheBackend = CacheBackend.CacheBackendSQLite
    , r2Config = None { r2Endpoint : Text, r2Bucket : Text }
    , lruCacheSize = +104857600
    }
|]

instructions :: Opts -> FilePath -> Text
instructions o wd
  = [i|hath-bench: fake RPC server on #{oPublicHost o}:#{oRpcPort o}. Start the client under test on #{oClientHost o}
(copy #{wd} there first if that is another machine):

  hs-hath:  cd #{wd} && HATH_RPC_HOST=#{oPublicHost o}:#{oRpcPort o} hs-hath --config ./hs-hath.dhall --skip-startup-verify
  Java:     cd #{wd}/java && java -jar /path/to/HentaiAtHome.jar --rpc_server_ip=#{oPublicHost o} --rpc_server_port=#{oRpcPort o}

(both configs use client id #{oClientId o}; pass --client-id/--client-key to change them)
Waiting for client_start...|]

mkGallery :: Int64 -> Int -> Int -> [ GFile ]
mkGallery nonce n size
  = [ GFile k fileId body
    | k <- [ 1 .. n ]
    , let prefix = encodeUtf8 ([i|hath-bench #{nonce} #{k} |] :: Text)
          body   = prefix <> BS.replicate (size - BS.length prefix) 46
          fileId = [i|#{hash @Text body}-#{size}-1280-1707-jpg|] ]

-- | RPC endpoint, fake image server (/up) and speed-test target (/t) on one port.
app :: Bench -> Wai.Application
app b req respond = case Wai.pathInfo req of
  [ "15", "rpc" ] -> rpc False
  -- 1.6.6+ (build 181): same actions, signed responses
  [ "16", "rpc" ] -> rpc True
  [ "up", fileId ] -> upstream fileId
  [ "t", sizeText, _, testKey, _ ] -> testTarget sizeText testKey
  _ -> respond $ Wai.responseLBS H.status404 [] ""
  where
    o = bOpts b

    -- Not Wai.queryString: parseQuery also splits on ';', and the Java client sends
    -- add=fileindex;xres;fileid unescaped.
    param k
      = maybe "" (H.urlDecode True . BS.drop (BS.length k + 1))
      $ find ((k <> "=") `BS.isPrefixOf`)
      $ BSC.split '&' (BS.dropWhile (== 63) (Wai.rawQueryString req))

    -- hs-hath's servant client insists on the real server's content type,
    -- the Java client on a Content-Length
    rpcResponse body
      = respond
      $ Wai.responseLBS
        H.status200
        [ ( H.hContentType, "text/plain;charset=UTF-8" ), ( H.hContentLength, show (BS.length body) ) ]
        (LBS.fromStrict body)

    -- 1.6.6 signs replies: OK-<t>-<sha1("<t>-<key>" ++ concatMap ("\n" ++) lines)>
    okWith signed ls = do
      now <- systemSeconds <$> getSystemTime
      let sig = hash @Text (encodeUtf8 (T.concat ([i|#{now}-#{oClientKey o}|] : map ("\n" <>) ls)))
          status = if signed then [i|OK-#{now}-#{sig}|] else "OK"
      rpcResponse (encodeUtf8 (T.unlines (status : ls)))

    settings
      = [ [i|rpc_server_ip=#{oPublicHost o}|]
        , [i|host=#{oClientHost o}|]
        , [i|port=#{oClientPort o}|]
          -- what the real server sends; Java stalls non-local transfers on throttle_bytes=0
        , "throttle_bytes=640000000"
        , "disable_bwm=true"
        , "disklimit_bytes=1073741824"
        , "diskremaining_bytes=1073741824" ]

    rpc signed = do
      let ok = okWith signed
      let act = decodeUtf8 (param "act") :: Text
      atomicModifyIORef' (bRpcCalls b) (\m -> ( Map.insertWith (+) act 1 m, () ))
      now <- systemSeconds <$> getSystemTime
      case act of
        "server_stat" -> ok [ "min_client_build=1", "cur_client_build=999", [i|server_time=#{now}|] ]
        "client_login" -> ok settings
        "client_settings" -> ok settings
        "get_cert" -> rpcResponse (bP12 b)
        "client_start" -> tryPutMVar (bStarted b) () >> ok []
        "srfetch" -> do
          atomicModifyIORef' (bSrfetch b) (\n -> ( n + 1, () ))
          -- add = fileindex;xres;fileid
          let fileId = decodeUtf8 (BSC.takeWhileEnd (/= ';') (param "add")) :: Text
          ok [ [i|http://#{oPublicHost o}:#{oRpcPort o}/up/#{fileId}|] ]
        _ -> ok [] -- still_alive, get_blacklist, client_stop, ...

    upstream fileId = case Map.lookup fileId (bFiles b) of
      Nothing -> respond $ Wai.responseLBS H.status404 [] ""
      Just f  -> do
        threadDelay (oUpDelayMs o * 1000)
        let body = gBytes f
        respond
          $ Wai.responseStream
            H.status200
            [ ( H.hContentType, "image/jpeg" ), ( H.hContentLength, show (BS.length body) ) ]
          $ \write flush -> forM_ (chunksOf 16384 body) $ \c -> do
            write (BB.byteString c) >> flush
            when (oUpKBps o > 0) $ threadDelay (BS.length c * 1000 `div` oUpKBps o)

    testTarget sizeText testKey = case readMaybe @Int (toString sizeText) of
      Nothing -> respond $ Wai.responseLBS H.status400 [] ""
      Just size
        | testKey == "broken403" -> respond $ Wai.responseLBS H.status403 [] ""
        -- no Content-Length and only half the bytes: the tester sees a clean, early EOF
        | testKey == "brokenshort" -> zeros [] (size `div` 2)
        | otherwise -> zeros [ ( H.hContentLength, show size ) ] size

    zeros headers n
      = respond
      $ Wai.responseStream H.status200 headers
      $ \write _ -> forM_ (chunksOf 65536 (BS.replicate n 48)) (write . BB.byteString)

chunksOf :: Int -> ByteString -> [ ByteString ]
chunksOf n bs
  | BS.null bs = []
  | otherwise = let ( a, rest ) = BS.splitAt n bs in a : chunksOf n rest

data Sample
  = Sample
  { sStatus :: !(Maybe Int) -- ^ Nothing: connection error or timeout
  , sTtfb   :: !Double      -- ^ seconds until response headers
  , sTotal  :: !Double      -- ^ seconds until the body is drained
  , sBytes  :: !Int
  , sHead   :: !ByteString  -- ^ first bytes of the body
  }

-- | GET with redirects followed, timing headers and body separately.
fetch :: HC.Manager -> String -> IO Sample
fetch mgr url = do
  t0 <- getMonotonicTime
  r <- timeout (90 * 1000000) $ try @SomeException $ do
    req <- HC.parseRequest url
    HC.withResponse req mgr $ \resp -> do
      t1 <- getMonotonicTime
      let drain !n acc = HC.brRead (HC.responseBody resp) >>= \c -> if BS.null c
            then pure ( n, acc )
            else drain (n + BS.length c) (if BS.length acc < 256 then acc <> BS.take 256 c else acc)
      ( n, headBytes ) <- drain 0 ""
      t2 <- getMonotonicTime
      pure $ Sample (Just (H.statusCode (HC.responseStatus resp))) (t1 - t0) (t2 - t0) n headBytes
  t3 <- getMonotonicTime
  pure $ case r of
    Just (Right s) -> s
    _ -> Sample Nothing (t3 - t0) (t3 - t0) 0 ""

data Row
  = Row
  { rowName    :: !Text
  , rowSamples :: ![ Sample ]
  , rowWall    :: !Double
  , rowPassed  :: !Bool
  , rowNote    :: !Text
  }

runPhases :: Bench -> HC.Manager -> [ GFile ] -> IO [ Row ]
runPhases b mgr files = do
  now <- systemSeconds <$> getSystemTime
  let o = bOpts b
      base = [i|https://#{oClientHost o}:#{oClientPort o}|] :: String
      sha1 = hash @Text . encodeUtf8 @Text

      servercmd :: Text -> Text -> String
      servercmd cmd add
        = let
            sig = sha1 [i|hentai@home-servercmd-#{cmd}-#{add}-#{oClientId o}-#{now}-#{oClientKey o}|]
          in 
            [i|#{base}/servercmd/#{cmd}/#{add}/#{now}/#{sig}|]

      proxyTest :: Text -> String
      proxyTest k
        = servercmd
          "threaded_proxy_test"
          [i|hostname=#{oPublicHost o};protocol=http;port=#{oRpcPort o};testsize=1000000;testcount=5;testtime=#{now};testkey=#{k}|]

      speedKey = sha1 [i|hentai@home-speedtest-5000000-#{now}-#{oClientId o}-#{oClientKey o}|]

      fileUrl f
        = let
            stamp = T.take 10 (sha1 [i|#{now}-#{gId f}-#{oClientKey o}-hotlinkthis|])
          in 
            [i|#{base}/h/#{gId f}/keystamp=#{now}-#{stamp};fileindex=#{gIndex f};xres=org/#{gIndex f}.jpg|]

      -- run n requests with the given concurrency; the check sees all samples
      phase name conc urls check = do
        t0 <- getMonotonicTime
        ss <- pooledMapConcurrentlyN conc (fetch mgr) urls
        t1 <- getMonotonicTime
        let ( passed, note ) = check ss
        pure $ Row name ss (t1 - t0) passed note

      allFull size ss = all (\s -> sStatus s == Just 200 && sBytes s == size) ss
      bodyText = decodeUtf8 . sHead :: Sample -> Text
      bodyIs p ss = ( all (\s -> sStatus s == Just 200 && p (bodyText s)) ss, T.intercalate " " (bodyText <$> ss) )

      galleryPhase name expectFetches = do
        before <- readIORef (bSrfetch b)
        row <- phase name 4 (fileUrl <$> files) (\ss -> ( allFull (oFileSize o) ss, "" ))
        fetches <- subtract before <$> readIORef (bSrfetch b)
        pure row { rowPassed = rowPassed row && fetches == expectFetches
                 , rowNote = [i|srfetch=#{fetches} (want #{expectFetches})|] }

  -- for poking the client from a third machine (e.g. a flood-control burst)
  writeFileText (oWorkDir o </> "urls.txt") (T.unlines (toText . fileUrl <$> files))
  sequence
    [ phase "servercmd still_alive" 1 [ servercmd "still_alive" "" ] (bodyIs (const True))
    , phase "servercmd speed_test" 2 (replicate 2 (servercmd "speed_test" "testsize=2500000"))
        (\ss -> ( allFull 2500000 ss, "" ))
    , phase "servercmd unsigned" 1 [ [i|#{base}/servercmd/still_alive//#{now}/0|] ]
        (\ss -> ( map sStatus ss == [ Just 403 ], "want 403" ))
    , phase "proxy_test healthy" 1 [ proxyTest "ok" ] (bodyIs ("OK:5-" `T.isPrefixOf`))
    , phase "proxy_test target 403" 1 [ proxyTest "broken403" ] (bodyIs (== "OK:0-0"))
    , phase "proxy_test short body" 1 [ proxyTest "brokenshort" ] (bodyIs (== "OK:0-0"))
    , phase "/t/ 5x5MB concurrent" 5
        [ [i|#{base}/t/5000000/#{now}/#{speedKey}/#{k}|] | k <- [ 1 .. 5 :: Int ] ]
        (\ss -> ( allFull 5000000 ss, "" ))
    , galleryPhase "/h/ cold (cache miss)" (length files)
    , galleryPhase "/h/ warm (cache hit)" 0
    ]

report :: Opts -> [ Row ] -> Map Text Int -> IO ()
report o rows calls = do
  printf "\nhath-bench results (client on %s:%d)\n\n" (toString (oClientHost o)) (oClientPort o)
  printf "%-24s %-14s %15s %15s %8s  %s\n"
    ("phase" :: String) ("status" :: String) ("ttfb p50/p95" :: String)
    ("total p50/p95" :: String) ("MB/s" :: String) ("check" :: String)
  forM_ rows $ \r -> do
    let ss = rowSamples r
        statuses = T.unwords
          [ [i|#{maybe "err" (show @Text) s}x#{c}|] :: Text
          | ( s, c ) <- Map.toList (Map.fromListWith (+) [ ( sStatus x, 1 :: Int ) | x <- ss ]) ]
        ms f = [i|#{msAt 0.5 f ss}/#{msAt 0.95 f ss}|] :: Text
        mbps = fromIntegral (sum (sBytes <$> ss)) / max 1e-9 (rowWall r) / 1e6 :: Double
    printf "%-24s %-14s %15s %15s %8.1f  %s %s\n"
      (toString (rowName r)) (toString statuses) (toString (ms sTtfb)) (toString (ms sTotal)) mbps
      (if rowPassed r then "ok" else "FAIL" :: String) (toString (rowNote r))
  printf "\nRPC calls seen: %s\n"
    (toString (T.unwords [ [i|#{a}x#{n}|] | ( a, n ) <- Map.toList calls ]))
  where
    msAt :: Double -> (Sample -> Double) -> [ Sample ] -> Int
    msAt p f ss = case sort (f <$> ss) of
      [] -> 0
      xs -> round (1000 * (xs List.!! min (length xs - 1) (floor (p * fromIntegral (length xs)))))
