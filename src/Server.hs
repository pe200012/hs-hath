module Server ( startServer ) where

import           Control.Concurrent      ( forkIO, threadDelay )

import qualified Data.Map.Strict         as Map
import           Data.String.Interpolate ( i )
import           Data.Time.Clock         ( UTCTime, addUTCTime, getCurrentTime )

import           Network.HTTP.Types      ( mkStatus )
import           Network.Socket          ( HostAddress, HostAddress6, SockAddr(..) )
import           Network.Wai             ( Middleware, Response, remoteHost, responseLBS )

import           Relude                  hiding ( get )

import           URLParam                ( parseURLParams )

import           Web.Scotty

-- Data types for tracking requests
data IPRecord
    = IPRecord { requestTimes :: [ UTCTime ]  -- Times of recent requests
               , bannedUntil  :: Maybe UTCTime  -- When the ban expires
               }
    deriving ( Show )

-- Change the Map key from SockAddr to a custom IP type
data IP = IPv4 HostAddress | IPv6 HostAddress6
    deriving ( Eq, Ord, Show )

type IPMap = Map.Map IP IPRecord

-- Helper function to extract just the IP from a SockAddr
getIP :: SockAddr -> Maybe IP
getIP (SockAddrInet _ ha) = Just $ IPv4 ha
getIP (SockAddrInet6 _ _ ha _) = Just $ IPv6 ha
getIP _ = Nothing

-- Create a new rate limiting middleware
rateLimitMiddleware :: TVar IPMap -> Middleware
rateLimitMiddleware ipMap app req respond = do
    now <- getCurrentTime
    let sockAddr = remoteHost req
    case getIP sockAddr of
        Nothing -> app req respond  -- If we can't get IP, just allow the request
        Just ip -> do
            allowed <- checkRateLimit ipMap ip now
            if allowed
                then app req respond
                else respond tooManyRequestsResponse

-- Check if request is allowed and update rate limit state
checkRateLimit :: TVar IPMap -> IP -> UTCTime -> IO Bool
checkRateLimit ipMap ip now = atomically $ do
    m <- readTVar ipMap
    maybe firstRequest existingRequest (Map.lookup ip m)
  where
    -- Handle first request from an IP by creating new record
    firstRequest = do
        modifyTVar' ipMap $ Map.insert ip (IPRecord [ now ] Nothing)
        return True

    -- Check if IP is banned, otherwise check request count
    existingRequest record = case bannedUntil record of
        Just banTime
            | banTime > now -> return False
        _ -> checkAndUpdateRequests record

    -- Check if requests are within rate limit window and update accordingly
    checkAndUpdateRequests record = do
        let windowStart    = addUTCTime (-10) now  -- 10 second window
            recentRequests = filter (> windowStart) (requestTimes record)
            newRequests    = now : recentRequests

        if length recentRequests >= 4
            then banIP newRequests
            else allowRequest newRequests

    -- Ban IP for 10 minutes if too many requests
    banIP newRequests = do
        let banUntil = addUTCTime (10 * 60) now  -- 10 minute ban
        modifyTVar' ipMap $ Map.insert ip (IPRecord newRequests (Just banUntil))
        return False

    -- Allow request and update record with new request time
    allowRequest newRequests = do
        modifyTVar' ipMap $ Map.insert ip (IPRecord newRequests Nothing)
        return True

{-# INLINE tooManyRequestsResponse #-}
-- Standard 429 response
tooManyRequestsResponse :: Response
tooManyRequestsResponse
    = responseLBS
        (mkStatus 429 "Too Many Requests")
        [ ( "Content-Type", "text/plain" ) ]
        "Too Many Requests"

-- Initialize the server with rate limiting
startServer :: Int -> IO ()
startServer port = do
    ipMap <- newTVarIO Map.empty

    -- Start cleanup thread
    void $ forkIO $ forever $ do
        threadDelay (60 * 1000000)  -- Clean every minute
        now <- getCurrentTime
        atomically $ modifyTVar' ipMap $ Map.filter $ \record -> case bannedUntil record of
            Just banTime
                | banTime <= now -> False  -- Remove expired bans
            _ -> True

    scotty port $ do
        middleware (rateLimitMiddleware ipMap)

        -- Resource handler
        get "/h/:info/:opts/:filename" $ do
            info <- pathParam @Text "info"
            rawOpts <- pathParam @ByteString "opts"
            filename <- pathParam @Text "filename"
            let opts = parseURLParams rawOpts
            text [i|Resource request: #{info}, #{opts}, #{filename}|]

        -- Server command handler
        get "/servercmd/:command/:additional/:time/:key" $ do
            command <- pathParam @Text "command"
            additional <- pathParam @Text "additional"
            time <- pathParam @Int "time"
            key <- pathParam @Text "key"
            text [i|Server command: #{command}, #{additional}, #{time}, #{key}|]

        -- Test handler
        get "/t/:testsize/:testtime/:testkey/:nothing" $ do
            testsize <- pathParam @Text "testsize"
            testtime <- pathParam @Int "testtime"
            testkey <- pathParam @Text "testkey"
            nothing <- pathParam @Text "nothing"
            text [i|Test request: #{testsize}, #{testtime}, #{testkey}, #{nothing}|]

        -- Reload configuration
        post "/reload" $ do
            text "Configuration reloaded"

        -- Default route
        get "/" $ do
            text "Hath Server Running"
