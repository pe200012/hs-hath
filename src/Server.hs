module Server ( startServer ) where

import           Control.Concurrent ( forkIO, threadDelay )

import qualified Data.Map.Strict    as Map
import           Data.Time.Clock    ( UTCTime, addUTCTime, getCurrentTime )

import           Network.HTTP.Types ( mkStatus )
import           Network.Socket     ( SockAddr(..), HostAddress, HostAddress6 )
import           Network.Wai        ( Middleware, remoteHost, responseLBS )

import           Relude             hiding ( get )

import           Web.Scotty

-- Data types for tracking requests
data IPRecord
    = IPRecord { requestTimes :: [ UTCTime ]  -- Times of recent requests
               , bannedUntil  :: Maybe UTCTime  -- When the ban expires
               }
    deriving ( Show )

-- Change the Map key from SockAddr to a custom IP type
data IP = IPv4 HostAddress | IPv6 HostAddress6
    deriving (Eq, Ord, Show)

type IPMap = Map.Map IP IPRecord

-- Helper function to extract just the IP from a SockAddr
getIP :: SockAddr -> Maybe IP
getIP (SockAddrInet _ ha)      = Just $ IPv4 ha
getIP (SockAddrInet6 _ _ ha _) = Just $ IPv6 ha
getIP _                        = Nothing

-- Create a new rate limiting middleware
rateLimitMiddleware :: TVar IPMap -> Middleware
rateLimitMiddleware ipMap app req respond = do
    now <- getCurrentTime
    let sockAddr = remoteHost req
    case getIP sockAddr of
        Nothing -> app req respond  -- If we can't get IP, just allow the request
        Just ip -> do
            -- Check and update IP status atomically
            allowed <- atomically $ do
                m <- readTVar ipMap
                case Map.lookup ip m of
                    Nothing -> do
                        -- First request from this IP
                        modifyTVar' ipMap $ Map.insert ip (IPRecord [now] Nothing)
                        return True

                    Just record -> case bannedUntil record of
                        Just banTime
                            | banTime > now ->
                                -- IP is banned
                                return False

                        _ -> do
                            -- Clean old requests and check rate limit
                            let windowStart    = addUTCTime (-10) now  -- 10 seconds ago
                                recentRequests = filter (> windowStart) (requestTimes record)
                                newRequests    = now : recentRequests

                            if length recentRequests >= 4  -- 5th request would exceed limit
                                then do
                                    -- Ban for 10 minutes
                                    let banUntil = addUTCTime (10 * 60) now
                                    modifyTVar' ipMap
                                        $ Map.insert ip (IPRecord newRequests (Just banUntil))
                                    return False
                                else do
                                    modifyTVar' ipMap $ Map.insert ip (IPRecord newRequests Nothing)
                                    return True
            if allowed
                then app req respond
                else respond
                $ responseLBS
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
        get "/" $ do
            text "Hello, World!"