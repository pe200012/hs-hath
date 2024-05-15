
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Server ( module Server ) where

import           Cache                                ( HathFile(..), parseHathFile, verifyCache )

import           Colog                                ( Message
                                                      , WithLog
                                                      , logError
                                                      , logInfo
                                                      , richMessageAction
                                                      , usingLoggerT
                                                      )

import           Control.Concurrent                   ( MVar
                                                      , forkIO
                                                      , newEmptyMVar
                                                      , putMVar
                                                      , takeMVar
                                                      , threadDelay
                                                      )
import           Control.Concurrent.Async             ( race )
import           Control.Exception                    ( SomeException )
import           Control.Monad                        ( forever
                                                      , replicateM
                                                      , replicateM_
                                                      , void
                                                      , when
                                                      )
import           Control.Monad.Catch                  ( MonadThrow )
import           Control.Monad.IO.Class               ( MonadIO(liftIO) )
import           Control.Monad.Reader                 ( asks )
import           Control.Monad.Trans                  ( lift )

import           Data.ByteString                      ( ByteString )
import           Data.ByteString.Base64               ( encodeBase64' )
import           Data.ByteString.Builder              ( byteString )
import qualified Data.ByteString.Char8                as BS
import qualified Data.ByteString.Lazy.Char8           as LBS
import           Data.Foldable                        ( for_ )
import qualified Data.HashMap.Strict                  as HashMap
import           Data.IORef                           ( IORef, modifyIORef', newIORef, readIORef )
import           Data.IORef.Extra                     ( writeIORef' )
import           Data.Int                             ( Int64 )
import           Data.Map                             ( Map )
import qualified Data.Map                             as Map
import           Data.Maybe                           ( fromMaybe )
import           Data.String.Interpolate              ( i )
import qualified Data.Text                            as Text
import           Data.Time.Clock.POSIX                ( POSIXTime, getPOSIXTime )
import           Data.Time.Clock.System               ( SystemTime(systemSeconds), getSystemTime )

import           Database.SQLite.Simple               ( withConnection )
import qualified Database.SQLite.Simple               as SQLite

import           Network.HTTP.Client                  ( Proxy(Proxy)
                                                      , Request(..)
                                                      , brReadSome
                                                      , newManager
                                                      , withResponse
                                                      )
import           Network.HTTP.Client.TLS              ( tlsManagerSettings )
import           Network.HTTP.Simple                  ( getResponseBody )
import           Network.HTTP.Types                   ( status200
                                                      , status301
                                                      , status403
                                                      , status404
                                                      , status418
                                                      )
import           Network.TLS                          ( Credential
                                                      , Credentials(Credentials)
                                                      , Version(..)
                                                      )
import           Network.TLS.Extra.Cipher             ( ciphersuite_default )
import           Network.Wai                          ( Application )
import           Network.Wai.Handler.Warp             ( defaultSettings, setPort )
import           Network.Wai.Handler.WarpTLS          ( TLSSettings(..), runTLS )
import           Network.Wai.Handler.WarpTLS.Internal ( defaultTlsSettings )
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev )

import           Prelude                              hiding ( log )

import           Query

import           Resource

import           Types

import           UnliftIO                             ( Handler(Handler)
                                                      , atomically
                                                      , modifyTVar'
                                                      , newTVarIO
                                                      , readTVarIO
                                                      , try
                                                      )

import           Utils

import           Web.Scotty.Trans                     ( pathParam )
import           Web.Scotty.Trans.Strict              ( ActionT
                                                      , Options(..)
                                                      , defaultHandler
                                                      , file
                                                      , get
                                                      , header
                                                      , headers
                                                      , notFound
                                                      , raw
                                                      , scottyAppT
                                                      , setHeader
                                                      , status
                                                      , stream
                                                      , text
                                                      )

handleRPCCommand :: ( MonadIO m, WithLog (Singleton (HathM m) Message) Message (HathM m) )
                 => ByteString
                 -> ByteString
                 -> ActionT (HathM m) ()
handleRPCCommand "still_alive" _ = text "I feel FANTASTIC and I'm still alive"
handleRPCCommand "refresh_settings" _ = do
    lift $ logInfo "Refreshing settings"
    cfg <- lift $ asks clientConfig
    newSts <- lift $ Query.hathSettings cfg
    stRef <- lift $ asks Types.hathSettings
    liftIO $ writeIORef' stRef newSts
    text ""
handleRPCCommand "refresh_certs" _ = do
    lift $ logInfo "Refreshing certificates"
    cfg <- lift $ asks clientConfig
    cred <- lift $ liftA2 fromPkcs12 (asks clientConfig) (hathCertificate cfg)
    text ""
    shutdown <- lift $ asks Types.refreshCertificate
    liftIO $ putMVar shutdown (RefreshCert cred)
handleRPCCommand "threaded_proxy_test" additional = do
    lift $ logInfo "Starting threaded speed test"
    testSpeed (parseOptions additional)
handleRPCCommand "speed_test" additional = do
    lift $ logInfo "Starting speed test"
    sendTestData testSize
  where
    opts     = parseOptions additional

    testSize = maybe 1000000 fst (BS.readInt =<< Map.lookup "testsize" opts)
handleRPCCommand _ _ = text "INVALID_COMMAND"

handleResourceRequest :: ( MonadIO m, MonadThrow m )
                      => ByteString
                      -> HathFile
                      -> Map ByteString ByteString
                      -> ActionT (HathM m) ()
handleResourceRequest filename fileid opts = lift (locateResource filename fileid opts) >>= \case
    Nothing    -> status status404
    Just bytes -> do
        setHeader "Content-Type" mime
        setHeader "Content-Disposition" [i|inline; filename="#{filename}"|]
        raw $ LBS.fromChunks $ pure bytes
  where
    mime = case fileType fileid of
        "jpg" -> "image/jpeg"
        "png" -> "image/png"
        "gif" -> "image/gif"
        "mp4" -> "video/mp4"
        "wbm" -> "video/webm"
        "wbp" -> "video/webp"
        "avf" -> "video/avif"
        "jxl" -> "image/jxl"
        _     -> "application/octet-stream"

sendTestData :: MonadIO m => Int -> ActionT (HathM m) ()
sendTestData testSize = do
    setHeader "Content-Type" "text/html; charset=iso-8859-1"
    setHeader "Content-Length" [i|#{testSize}|]
    stream $ \sender flushing -> do
        replicateM_ loopTimes $ do
            sender $ byteString preallocate
        sender $ byteString $ BS.take rest preallocate
        flushing
  where
    ( loopTimes, rest ) = testSize `divMod` tcpPacketSize

    preallocate = BS.pack (replicate tcpPacketSize '0')

    {-# NOINLINE preallocate #-}

tcpPacketSize :: Int
tcpPacketSize = 1460

testSpeed :: ( MonadIO m, WithLog (Singleton (HathM m) Message) Message (HathM m) )
          => Map ByteString ByteString
          -> ActionT (HathM m) ()
testSpeed opts = case testArgs of
    Nothing -> text "Invalid test arguments"
    Just ( url, count, siz ) -> do
        lift $ logInfo [i|Starting speed test with #{count} requests|]
        pxy <- asks (clientProxy . clientConfig)
        mgr <- liftIO $ newManager tlsManagerSettings
        successCount <- liftIO $ newTVarIO (0 :: Int)
        startTime <- liftIO getPOSIXTime
        liftIO $ for_ [ 1 .. count ] $ \k -> do
            void $ try @IO @SomeException $ do
                pullTestData siz mgr $ setReqProxy pxy [i|#{url}/#{k}|]
                atomically (modifyTVar' successCount (+ 1))
        endTime <- liftIO getPOSIXTime
        success <- liftIO $ readTVarIO successCount
        let totalTime = realToFrac @POSIXTime @Double $ 1000 * (endTime - startTime)
        lift $ logInfo [i|Speed test completed in #{totalTime}ms, #{success} requests|]
        text [i|OK:#{success}-#{truncate totalTime :: Int}|]
  where
    pullTestData siz mgr req = withResponse req mgr (void . flip brReadSome siz . getResponseBody)

    setReqProxy pxy r
        = r
        { proxy = (\(ClientProxy h p _)
                   -> Proxy h p) <$> pxy, requestHeaders = case proxyAuth =<< pxy of
              Nothing -> []
              Just ( user, pass )
                  -> [ ( "Proxy-authorization", [i|Basic #{encodeBase64' (user <> ":" <> pass)}|] )
                     , ( "Connection", "close" )
                     , ( "User-Agent", "Hentai@Home 161" )
                     , ( "Cache-Control", "public, max-age=31536000" )
                     , ( "Server", "Genetic Lifeform and Distributed Open Server 1.6.2" )
                     , ( "Proxy-Connection", "Keep-Alive" )
                     ] }

    testArgs :: Maybe ( ByteString, Int, Int )
    testArgs = do
        hostname <- Map.lookup "hostname" opts
        testPort <- Map.lookup "port" opts
        let protocol = fromMaybe "http" $ Map.lookup "protocol" opts
        ( testSize, _ ) <- BS.readInt =<< Map.lookup "testsize" opts
        testTime <- Map.lookup "testtime" opts
        ( testCount, _ ) <- BS.readInt =<< Map.lookup "testcount" opts
        testKey <- Map.lookup "testkey" opts
        return
            ( [i|#{protocol}://#{hostname}:#{testPort}/t/#{testSize}/#{testTime}/#{testKey}|]
            , testCount
            , testSize
            )

maxTimeDrift :: Int64
maxTimeDrift = 300

runHTTPServer :: IORef HathSettings -> ClientConfig -> IO ()
runHTTPServer hSets config = do
    sts <- readIORef hSets
    print sts
    writeIORef' hSets sts
    cred <- usingLoggerT richMessageAction (fromPkcs12 config <$> hathCertificate config)
    terminateRequest <- newEmptyMVar
    credRef <- newIORef cred
    flood <- newIORef (HashMap.empty @ByteString @Int)
    void $ forkIO $ forever (threadDelay (5 * 60 * 1000000) >> writeIORef' flood HashMap.empty)
    withConnection "./cache.db" $ \conn -> do
        SQLite.execute_ conn "pragma journal_mode=WAL"
        SQLite.execute_ conn "pragma synchronous=normal"
        SQLite.execute_ conn "pragma temp_store=memory"
        SQLite.execute_ conn "pragma cache_size=100000"
        SQLite.execute_ conn "pragma mmap_size=65536"
        runHath config hSets conn terminateRequest credRef verifyCache
        app <- scottyAppT
            (Options { verbose = 1, settings = setPort (clientPort sts) defaultSettings })
            (runHath config hSets conn terminateRequest credRef)
            $ do
                defaultHandler $ Handler $ \(e :: SomeException) -> do
                    reqHeader <- headers
                    lift $ logError [i|Unhandled exception: #{e}, headers: #{reqHeader}|]
                get "/" $ status status404
                get "/favicon.ico" $ do
                    status status301
                    setHeader "Location" "https://e-hentai.org/favicon.ico"
                    text ""
                get "/robots.txt" $ do
                    status status200
                    text "User-agent: *\nDisallow: /"
                get "/h/:info/:opts/:filename" $ do
                    setCommonHeader
                    info <- pathParam @ByteString "info"
                    opts <- pathParam @ByteString "opts"
                    filename <- pathParam @ByteString "filename"
                    floodMap <- liftIO $ readIORef flood
                    currentTime <- liftIO getSystemTime
                    case parseHathFile info of
                        Nothing -> do
                            status status404
                            text "Invalid or missing file info"
                        Just f  -> let
                            keystamp   = fromMaybe "" $ Map.lookup "keystamp" (parseOptions opts)
                            floodCount = HashMap.lookupDefault 0 keystamp floodMap
                            in 
                                if
                                    | floodCount > 5 -> do
                                        status status418
                                        text "Hi there, you're flooding me"
                                    -- | invalidKeystamp keystamp info currentTime (clientKey config)
                                    --     -> do
                                    --         lift $ logWarning [i|Invalid keystamp: #{keystamp}|]
                                    --         status status403
                                    --         text "Keystamp verification failed"
                                    | otherwise -> do
                                        when (keystamp /= "")
                                            $ liftIO
                                            $ modifyIORef' flood
                                            $ HashMap.alter (maybe (pure 0) (pure . succ)) keystamp
                                        handleResourceRequest filename f (parseOptions opts)
                get "/servercmd/:command/:additional/:time/:key" $ do
                    setCommonHeader
                    cmd <- pathParam @ByteString "command"
                    add <- pathParam @ByteString "additional"
                    key <- pathParam @ByteString "key"
                    time <- pathParam @ByteString "time"
                    if key
                        /= hathHash
                            [i|hentai@home-servercmd-#{cmd}-#{add}-#{clientID config}-#{time}-#{clientKey config}|]
                        then status status403
                        else handleRPCCommand cmd add
                get "/t/:testsize/:testtime/:testkey/:nothing" $ do
                    setCommonHeader
                    testSize <- pathParam @Int "testsize"
                    testTime <- pathParam @Int64 "testtime"
                    testKey <- pathParam @ByteString "testkey"
                    currentTime <- liftIO getSystemTime
                    let cid  = clientID config
                        ckey = clientKey config
                    if
                        | abs (systemSeconds currentTime - testTime)
                          > maxTimeDrift -> status status403
                        | testKey
                          /= hathHash
                              [i|hentai@home-speedtest-#{testSize}-#{testTime}-#{cid}-#{ckey}|]
                            -> status status403
                        | otherwise -> sendTestData testSize
                notFound $ do
                    status status403
                    -- check if they accept gzip
                    acceptGzip <- header "Accept-Encoding"
                    when (maybe False (Text.isInfixOf "gzip") acceptGzip) $ do
                        -- try to give them a "surprise"
                        setCommonHeader
                        lift $ logInfo "Sending 100G.gzip"
                        status status200
                        setHeader "Content-Encoding" "gzip"
                        setHeader "Content-Type" "text/html"
                        file "100G.gzip"
        loop terminateRequest app credRef
  where
    invalidKeystamp :: ByteString -> ByteString -> SystemTime -> ByteString -> Bool
    invalidKeystamp ks fileid currentTime ckey
        = let
            ( rawTime, rest ) = BS.span (/= '-') ks
            keystampTime      = maybe 0 fst (BS.readInt rawTime)
            keystamp          = BS.drop 1 rest
            in 
                abs (systemSeconds currentTime - fromIntegral keystampTime) > 300
                || keystamp
                /= BS.take 10 (hathHash [i|#{keystampTime}-#{fileid}-#{ckey}-hotlinkthis|])

    setCommonHeader = do
        setHeader "Connection" "close"
        setHeader "User-Agent" "Hentai@Home 161"
        setHeader "Cache-Control" "public, max-age=31536000"
        setHeader "Server" "Server: Genetic Lifeform and Distributed Open Server 1.6.2"
        setHeader "X-Content-Type-Options" "nosniff"

    loop :: MVar RefreshCert -> Application -> IORef Credential -> IO ()
    loop terminateRequest router credRef = do
        cred <- readIORef credRef
        cPort <- clientPort <$> readIORef hSets
        res <- race
            (runTLS
                 (defaultTlsSettings { tlsCredentials     = Just (Credentials [ cred ])
                                     , tlsAllowedVersions = [ TLS13, TLS12, TLS10, TLS11 ]
                                     , tlsCiphers         = ciphersuite_default
                                     })
                 (setPort cPort defaultSettings)
                 (logStdoutDev router))
            (takeMVar terminateRequest)
        case res of
            Left _ -> error "impossible"
            Right (RefreshCert newCred) -> do
                writeIORef' credRef newCred
                putStrLn "Restarting server with new credentials"
                loop terminateRequest router credRef



