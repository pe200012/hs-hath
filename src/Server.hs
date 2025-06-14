{-# LANGUAGE DataKinds #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Server ( startServer, ServerAction(..) ) where

import           API                                  ( API
                                                      , ServerCommand(..)
                                                      , WithDynamicContentType(WithDynamicContentType)
                                                      , api
                                                      , fetchBlacklist
                                                      , runEHentaiAPI
                                                      , runEHentaiAPIIO
                                                      , startListening
                                                      , stopListening
                                                      )

import           Colog                                ( Message
                                                      , Severity(Info)
                                                      , richMessageAction
                                                      )
import           Colog.Polysemy                       ( Log, runLogAction )

import           Control.Concurrent                   ( ThreadId, forkIO, threadDelay, throwTo )
import           Control.Concurrent.STM.TVar          ( modifyTVar' )
import           Control.Concurrent.Suspend           ( mDelay )
import           Control.Concurrent.Timer             ( repeatedTimer, stopTimer )
import           Control.Exception                    ( try )

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as BSC
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Char                            ( isDigit, isSpace )
import qualified Data.HashMap.Strict                  as HashMap
import           Data.List                            ( nub )
import qualified Data.Map.Strict                      as Map
import           Data.String.Interpolate              ( i )
import qualified Data.Text                            as Text
import           Data.Time.Clock                      ( NominalDiffTime
                                                      , UTCTime
                                                      , addUTCTime
                                                      , getCurrentTime
                                                      )
import           Data.Time.Clock.POSIX                ( POSIXTime, getPOSIXTime )
import           Data.Time.Clock.System               ( SystemTime(systemSeconds), getSystemTime )
import           Data.X509                            ( CertificateChain, PrivKey )

import           Database                             ( initializeDB, runCache )
import           Database.SQLite.Simple               ( Connection, withConnection )

import           Genesis

import           Hash                                 ( hash )

import           Locate

import           Network.HTTP.Client                  ( brReadSome
                                                      , httpLbs
                                                      , newManager
                                                      , parseRequest
                                                      , responseBody
                                                      , withResponse
                                                      )
import           Network.HTTP.Client.TLS              ( tlsManagerSettings )
import           Network.HTTP.Simple                  ( getResponseBody )
import           Network.HTTP.Types                   ( hAccept, mkStatus, status200, status404 )
import           Network.Socket                       ( HostAddress, HostAddress6, SockAddr(..) )
import           Network.TLS                          ( Credentials(Credentials) )
import           Network.Wai                          ( Middleware
                                                      , Request(requestMethod)
                                                      , Response
                                                      , rawPathInfo
                                                      , remoteHost
                                                      , requestHeaders
                                                      , responseLBS
                                                      )
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Handler.Warp             ( defaultSettings, setPort )
import           Network.Wai.Handler.WarpTLS          ( OnInsecure(AllowInsecure)
                                                      , TLSSettings(..)
                                                      , defaultTlsSettings
                                                      , runTLS
                                                      )
import           Network.Wai.Middleware.RequestLogger ( logStdout, logStdoutDev )

import           Polysemy                             ( Embed
                                                      , Members
                                                      , Sem
                                                      , embed
                                                      , embedToFinal
                                                      , runFinal
                                                      )
import           Polysemy.Error                       ( Error, errorToIOFinal, mapError, throw )
import           Polysemy.Reader                      ( Reader, ask, runReader )

import           RPC                                  ( RPC, runRPC, runRPCIO, stillAlive )

import           Relude                               hiding ( Reader, ask, get, runReader )

import           Servant
import           Servant.Client                       ( ClientError )
import qualified Servant.Types.SourceT                as Source

import           SpeedTest                            ( bufferSending )

import           Types                                ( ClientConfig
                                                      , ClientConfig(..)
                                                      , FileURI(fileExt)
                                                      , HathSettings(..)
                                                      , RPCError
                                                      , hentaiHeader
                                                      , parseFileURI
                                                      )

import           URLParam                             ( lookupParam, parseURLParams )

import           UnliftIO                             ( race, withAsync )

import           Utils                                ( log )

maxTimeDrift :: Int64
maxTimeDrift = 600

timeWindow :: NominalDiffTime
timeWindow = 10

maxRequests :: Int
maxRequests = 5

data ServerAction = Reload | Cert | Settings | GracefulShutdown

-- Data types for tracking requests
data IPRecord
    = IPRecord { requestTimes :: {-# UNPACK #-} ![ UTCTime ]  -- Times of recent requests
               , bannedUntil  :: {-# UNPACK #-} !(Maybe UTCTime)  -- When the ban expires
               }
    deriving ( Show )

-- Change the Map key from SockAddr to a custom IP type
data IP = IPv4 {-# UNPACK #-} !HostAddress | IPv6 {-# UNPACK #-} !HostAddress6
    deriving ( Eq, Ord, Show, Generic )

instance Hashable IP

type IPMap = HashMap IP IPRecord

{-# INLINE getIP #-}
-- Helper function to extract just the IP from a SockAddr
getIP :: SockAddr -> Maybe IP
getIP (SockAddrInet _ ha) = Just $ IPv4 ha
getIP (SockAddrInet6 _ _ ha _) = Just $ IPv6 ha
getIP _ = Nothing

-- Create a new rate limiting middleware
rateLimitMiddleware :: TVar IPMap -> Middleware
rateLimitMiddleware ipMap app req k
    | "/h/" `BS.isPrefixOf` rawPathInfo req = do
        now <- getCurrentTime
        let sockAddr = remoteHost req
        case getIP sockAddr of
            Nothing -> app req k  -- If we can't get IP, just allow the request
            Just ip -> do
                allowed <- checkRateLimit ipMap ip now
                if allowed
                    then app req k
                    else k tooManyRequestsResponse
    | otherwise = app req k  -- Skip rate limiting for non-resource paths

-- Check if request is allowed and update rate limit state
checkRateLimit :: TVar IPMap -> IP -> UTCTime -> IO Bool
checkRateLimit ipMap ip now = atomically $ do
    m <- readTVar ipMap
    maybe firstRequest existingRequest (HashMap.lookup ip m)
  where
    -- Handle first request from an IP by creating new record
    firstRequest = do
        modifyTVar' ipMap $ HashMap.insert ip (IPRecord [ now ] Nothing)
        return True

    -- Check if IP is banned, otherwise check request count
    existingRequest record = case bannedUntil record of
        Just banTime
            | banTime > now -> return False
        _ -> checkAndUpdateRequests record

    -- Check if requests are within rate limit window and update accordingly
    checkAndUpdateRequests record = do
        let windowStart    = addUTCTime (-timeWindow) now
            recentRequests = takeWhile (> windowStart) (requestTimes record)
            newRequests    = now : recentRequests

        if length recentRequests >= maxRequests
            then banIP newRequests
            else allowRequest newRequests

    -- Ban IP for 10 minutes if too many requests
    banIP newRequests = do
        let banUntil = addUTCTime (10 * 60) now  -- 10 minute ban
        modifyTVar' ipMap $ HashMap.insert ip (IPRecord newRequests (Just banUntil))
        return False

    -- Allow request and update record with new request time
    allowRequest newRequests = do
        modifyTVar' ipMap $ HashMap.insert ip (IPRecord newRequests Nothing)
        return True

{-# INLINE tooManyRequestsResponse #-}
-- Standard 429 response
tooManyRequestsResponse :: Response
tooManyRequestsResponse
    = responseLBS
        (mkStatus 429 "Too Many Requests")
        [ ( "Content-Type", "text/plain" ) ]
        "Too Many Requests"

-- Server implementation
server :: Members
           '[ Embed IO
            , Error ServerError
            , Reader ClientConfig
            , RPC
            , Locate
            , Reader (MVar ServerAction)
            , Log Message
            ]
           r
       => ServerT API (Sem r)
server
    = faviconHandler
    :<|> robotsHandler
    :<|> resourceHandler
    :<|> serverCmdHandler
    :<|> testHandler
    :<|> Tagged rawHandler
  where
    faviconHandler
        = throw $ err301 { errHeaders = [ ( "Location", "https://e-hentai.org/favicon.ico" ) ] }

    robotsHandler = return "User-agent: *\nDisallow: /"

    resourceHandler
        (encodeUtf8 -> fileid)
        (parseURLParams . encodeUtf8 -> opts)
        (encodeUtf8 -> filename) = do
        currentTime <- embed getSystemTime
        cfg <- ask @ClientConfig
        when (abs (timestamp - systemSeconds currentTime) > maxTimeDrift)
            $ throw
                err403 { errBody = "Your time is out of sync. Please update your system time." }
        when (answer /= challange cfg) $ throw err403 { errBody = "Invalid key." }
        res <- locateResource
            LocateURI { locateURIFilename = filename, locateURI = uri, locateURIOptions = opts }
        case res of
            Nothing -> throw err404
            Just bs -> return
                $ addHeader @"Content-Length" (BS.length bs)
                $ WithDynamicContentType mimeType bs
      where
        mimeType = case fileExt uri of
            "jpg" -> "image/jpeg"
            "png" -> "image/png"
            "gif" -> "image/gif"
            "mp4" -> "video/mp4"
            "wbm" -> "video/webm"
            "wbp" -> "video/webp"
            "avf" -> "video/avif"
            "jxl" -> "image/jxl"
            _     -> "application/octet-stream"

        uri = parseFileURI fileid

        ( timestamp :: Int64, answer )
            = let
                rs        = fromMaybe "" (Map.lookup "keystamp" opts)
                ( t, rk ) = BSC.span (/= '-') rs
                in 
                    ( fromIntegral $ maybe 0 fst $ BSC.readInteger t, BS.tail rk )

        {-# INLINE challange #-}
        challange :: ClientConfig -> ByteString
        challange cfg = BS.take 10 (hash [i|#{timestamp}-#{fileid}-#{key cfg}-hotlinkthis|])

    serverCmdHandler command (parseURLParams . encodeUtf8 -> additional) time key = case command of
        StillAlive        -> plainText "I feel FANTASTIC and I'm still alive"
        ThreadedProxyTest -> let
            args = do
                hostname <- lookupParam "hostname" additional
                protocol <- lookupParam "protocol" additional <|> return "http"
                port <- lookupParam "port" additional
                testSize <- readInt @Int64 =<< lookupParam "testsize" additional
                testCount <- readInt =<< lookupParam "testcount" additional
                testTime <- readInt @Int64 =<< lookupParam "testtime" additional
                testKey <- lookupParam "testkey" additional
                return
                    ( testCount
                    , testSize
                    , [i|#{protocol}://#{hostname}:#{port}/t/#{testSize}/#{testTime}/#{testKey}/0|]
                    )
            in 
                case args of
                    Nothing -> throw err403
                    Just ( testCount, testSize, url ) -> do
                        mgr <- embed $ newManager tlsManagerSettings
                        case parseRequest url of
                            Nothing  -> throw err403
                            Just req -> do
                                results <- replicateM testCount $ embed $ runTest testSize req mgr
                                let ( failed :: Int, millis :: Int64 )
                                        = foldl' (\( f, t ) r -> case r of
                                                      Nothing -> ( f + 1, t )
                                                      Just v  -> ( f, t + v )) ( 0, 0 ) results
                                plainText [i|OK:#{failed}-#{millis}|]
        SpeedTest         -> let
            testSize
                = maybe
                    1000000
                    (fromIntegral . fst)
                    (BSC.readInteger =<< lookupParam "testsize" additional)
            in 
                return
                $ addHeader @"Content-Length" testSize
                $ Source.fromStepT
                $ bufferSending testSize
        RefreshSettings   -> do
            log Info "Refreshing settings"
            chan <- ask @(MVar ServerAction)
            void $ embed $ forkIO $ do
                threadDelay (1000000 * 2)
                putMVar chan Settings
            plainText ""
        StartDownloader   -> plainText ""
        RefreshCerts      -> do
            log Info "Refreshing certificates"
            chan <- ask @(MVar ServerAction)
            void $ embed $ forkIO $ do
                threadDelay (1000000 * 2)
                putMVar chan Cert
            plainText ""
      where
        runTest testSize req mgr = do
            start <- getPOSIXTime
            let phi 0 _  = do
                    end <- getPOSIXTime
                    return $ truncate $ realToFrac @POSIXTime @Double $ 1000 * (end - start)
                phi n br = do
                    s <- LBS.length <$> brReadSome br 1024
                    phi (max 0 (n - s)) br
            try @SomeException (withResponse req mgr (phi testSize . getResponseBody)) >>= \case
                Left _  -> return Nothing
                Right v -> return $ Just v

        {-# INLINE readInt #-}
        {-# SPECIALISE readInt :: ByteString -> Maybe Int #-}
        {-# SPECIALISE readInt :: ByteString -> Maybe Int64 #-}
        readInt :: Num a => ByteString -> Maybe a
        readInt = fmap (fromIntegral . fst) . BSC.readInteger

        {-# INLINE plainText #-}
        plainText t
            = pure
            $ addHeader @"Content-Length" (BS.length t)
            $ Source.fromStepT
            $ Source.Yield t Source.Stop

    testHandler testSize testTime (encodeUtf8 @_ @ByteString -> testKey) _ = do
        currentTime <- embed getSystemTime
        cfg <- ask @ClientConfig
        when (abs (testTime - systemSeconds currentTime) > maxTimeDrift) $ throw err403
        when (testKey /= challange cfg) $ throw err403
        return $ addHeader @"Content-Length" testSize $ Source.fromStepT $ bufferSending testSize
      where
        {-# INLINE challange #-}
        challange :: ClientConfig -> ByteString
        challange cfg
            = hash
                @ByteString
                [i|hentai@home-speedtest-#{testSize}-#{testTime}-#{clientId cfg}-#{key cfg}|]

    rawHandler req k = case requestMethod req of
        -- Handle HEAD requests with 200 OK and empty body
        "HEAD" -> k $ responseLBS status200 hentaiHeader ""

        _      -> k $ responseLBS status404 [] ""

tictok config ipMap = do
    -- ipmap cleanup
    cleanupHandle <- repeatedTimer
        (do
             now <- getCurrentTime
             atomically $ do
                 m <- readTVar ipMap
                 let windowStart = addUTCTime (-timeWindow) now
                 let filtered = HashMap.filter (\record -> case bannedUntil record of
                                                    Just banTime
                                                        | banTime > now -> True
                                                    _ -> case requestTimes record of
                                                        (t : _) -> t > windowStart
                                                        []      -> False) m
                 writeTVar ipMap filtered)
        (mDelay 1)

    -- heartbeat
    heartbeatHandle <- repeatedTimer (void $ runRPCIO config stillAlive) (mDelay 1)

    -- resource blacklist
    blacklistHandle <- repeatedTimer (do
                                          res <- runRPCIO config (fetchBlacklist 43200)
                                          print res) (mDelay 36)

    return $ \() -> do
        stopTimer cleanupHandle
        stopTimer heartbeatHandle
        stopTimer blacklistHandle

notifyStart :: ClientConfig -> HathSettings -> IO ()
notifyStart config _ = psi
  where
    psi = runRPCIO config startListening >>= \case
        Right (Right True) -> return ()
        _ -> threadDelay 1000000 >> psi

-- Create the WAI application with rate limiting
makeApplication
    :: ClientConfig -> HathSettings -> MVar ServerAction -> TVar IPMap -> Connection -> Application
makeApplication config settings action ipMap conn
    = rateLimitMiddleware ipMap
    $ normalizeAcceptMiddleware
    $ serve api (hoistServer api interpretServer server)
  where
    interpretServer :: Sem _ a -> Handler a
    interpretServer
        = Handler
        . ExceptT
        . runFinal
        . embedToFinal @IO
        . errorToIOFinal @ServerError
        . mapError @RPCError (const err500)
        . mapError @ClientError (const err500)
        . mapError @SomeException (const err500)
        . runReader settings
        . runReader config
        . runReader action
        . runLogAction @IO @Message richMessageAction
        . runEHentaiAPI
        . runCache conn
        . runLocate
        . runRPC

startServer
    :: ClientConfig -> HathSettings -> ( CertificateChain, PrivKey ) -> MVar ServerAction -> IO ()
startServer config settings certs chan = do
    ipMap <- newTVarIO HashMap.empty
    print =<< runRPCIO config (fetchBlacklist 259200)
    withConnection (Text.unpack $ cachePath config) $ \conn -> do
        initializeDB conn
        void $ tictok config ipMap
        loop config settings certs conn ipMap
  where
    gracefulShutdown :: IO a
    gracefulShutdown
        = let
            phi = flip unless phi . fromRight False =<< runEHentaiAPIIO config stopListening
            in 
                phi >> exitSuccess

    refreshSettings = phi 0
      where
        phi (retries :: Int) = runGenesisIO config fetchSettings >>= \case
            Right (Right newSetings) -> return newSetings
            e -> do
                putStrLn $ "Failed to refresh settings: " <> show e
                if retries > 3
                    then do
                        putStrLn "Giving up"
                        gracefulShutdown
                    else do
                        putStrLn $ "Retrying in " <> show (2 ^ retries :: Int) <> " seconds"
                        threadDelay (1000000 * 2 ^ retries)
                        phi (retries + 1)

    refreshCerts = phi 0
      where
        phi (retries :: Int) = runGenesisIO config fetchCertificate >>= \case
            Right (Right newCerts) -> return newCerts
            e -> do
                putStrLn $ "Failed to refresh certs: " <> show e
                if retries > 3
                    then do
                        putStrLn "Giving up"
                        gracefulShutdown
                    else do
                        putStrLn $ "Retrying in " <> show (2 ^ retries :: Int) <> " seconds"
                        threadDelay (1000000 * 2 ^ retries)
                        phi (retries + 1)

    loop cfg set c conn ipMap = do
        let app = makeApplication cfg set chan ipMap conn
        result <- withAsync (notifyStart cfg set) $ \_ -> race (takeMVar chan)
            $ runTLS
                (defaultTlsSettings
                 { tlsCredentials = Just (Credentials [ c ]), onInsecure = AllowInsecure })
                (setPort (clientPort set) defaultSettings)
            $ logStdoutDev
            $ logStdout app
        case result of
            Left GracefulShutdown -> gracefulShutdown
            Left Reload -> exitSuccess
            Left Cert -> do
                newCerts <- refreshCerts
                loop cfg set newCerts conn ipMap
            Left Settings -> do
                newSettings <- refreshSettings
                loop cfg newSettings certs conn ipMap
            Right _ -> error "Server terminated unexpectedly"

normalizeAcceptMiddleware :: Middleware
normalizeAcceptMiddleware app req = app req { requestHeaders = normalizedHeaders }
  where
    normalizedHeaders = map normalizeHeader (requestHeaders req)

    normalizeHeader ( name, value )
        | name == hAccept = ( name, "*/*" )
        | otherwise = ( name, value )

