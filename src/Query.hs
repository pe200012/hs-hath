
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Query ( module Query ) where

import           Colog                      ( Message, WithLog, logInfo )

import           Control.Monad              ( void, when )
import           Control.Monad.IO.Class     ( MonadIO(liftIO) )
import           Control.Monad.Reader       ( asks )

import           Crypto.Store.PKCS12        ( readP12FileFromMemory
                                            , recover
                                            , recoverAuthenticated
                                            , toCredential
                                            , toProtectionPassword
                                            )

import           Data.Bool                  ( bool )
import           Data.ByteString            ( ByteString )
import           Data.ByteString.Base64     ( encodeBase64' )
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Foldable              ( Foldable(foldl') )
import qualified Data.HashSet               as HashSet
import           Data.Int                   ( Int64 )
import           Data.String.Interpolate    ( i )
import           Data.Time.Clock.System     ( SystemTime(systemSeconds), getSystemTime )
import           Data.X509                  ( CertificateChain, PrivKey )

import           Network.HTTP.Client        ( Proxy(Proxy)
                                            , Request(..)
                                            , Response
                                            , parseRequest
                                            , responseTimeoutMicro
                                            , setQueryString
                                            , setRequestCheckStatus
                                            , setRequestIgnoreStatus
                                            )
import qualified Network.HTTP.Simple        as Simple
import           Network.HTTP.Simple        ( getResponseBody, getResponseStatus )
import           Network.HTTP.Types         ( Status )

import           Prelude                    hiding ( log )

import           Result                     ( RPCResult(..), StatusCode(..), parseRPCResult )

import           Text.Read                  ( readMaybe )

import           Types

import           Utils

baseURL :: ByteString
baseURL = "rpc.hentaiathome.net"

{-# NOINLINE baseURL #-}

getURLString :: ClientConfig -> ByteString -> ByteString -> SystemTime -> Request -> Request
getURLString (ClientConfig { .. }) action additional time initReq
    = setQueryString
        [ ( "act", Just action )
        , ( "cid", Just clientID )
        , ( "add"
          , if BS.null additional
                then Nothing
                else Just additional
          )
        , ( "acttime", Just [i|#{timeInSeconds}|] )
        , ( "actkey", Just key )
        , ( "clientbuild", Just clientVersion )
        ]
        initReq { method = "GET", responseTimeout = responseTimeoutMicro (60 * 1000000) }
  where
    timeInSeconds = systemSeconds time

    key
        = hathHash
            [i|hentai@home-#{action}-#{additional}-#{clientID}-#{timeInSeconds}-#{clientKey}|]

{-# INLINE getURLString #-}

serverStatus :: ( MonadIO m, WithLog env Message m ) => m Status
serverStatus = do
    logInfo "Requesting server status"
    res <- do
        initReq <- liftIO $ parseRequest (BS.unpack ("http://" <> baseURL <> "/15/rpc"))
        let req
                = setRequestCheckStatus
                $ setQueryString
                    [ ( "act", Just "server_stat" ), ( "clientbuild", Just "169" ) ]
                    initReq { method = "GET", requestHeaders = commonHeader }
        Simple.httpLbs req
    return $ getResponseStatus res

rpcQueryIO :: MonadIO m
           => ClientConfig
           -> ByteString
           -> ByteString
           -> ByteString
           -> Bool
           -> Bool
           -> m (Response LBS.ByteString)
rpcQueryIO (ClientConfig { .. }) rpcEndpoint action additional errOnStatus setProxy = do
    currentTime <- liftIO getSystemTime
    initReq <- liftIO $ parseRequest [i|http://#{baseURL}#{rpcEndpoint}|]
    let normalHeader  = commonHeader
        setReqProxy r
            = if setProxy
                then r
                    { proxy          = (Proxy . proxyHost <*> proxyPort) <$> clientProxy
                    , requestHeaders = case proxyAuth =<< clientProxy of
                          Nothing -> normalHeader
                          Just ( user, pass ) -> ( "Proxy-authorization"
                                                 , [i|Basic #{encodeBase64' (user <> ":" <> pass)}|]
                                                 )
                              : normalHeader
                    }
                else r
        req
            = bool setRequestIgnoreStatus setRequestCheckStatus errOnStatus
            $ setReqProxy
            $ getURLString
                (ClientConfig { .. })
                action
                additional
                currentTime
                initReq { method = "GET", responseTimeout = responseTimeoutMicro (60 * 1000000) }
    res <- Simple.httpLbs req
    when (LBS.length (getResponseBody res) < 1000) $ liftIO $ LBS.putStrLn $ getResponseBody res
    return res

rpcQuery
    :: MonadIO m => ByteString -> ByteString -> Bool -> Bool -> HathM m (Response LBS.ByteString)
rpcQuery action additional errOnStatus setProxy = do
    cfg <- asks clientConfig
    liftIO $ rpcQueryIO cfg "/15/rpc" action additional errOnStatus setProxy

rpcQuery' :: MonadIO m => ByteString -> Bool -> Bool -> HathM m (Response LBS.ByteString)
rpcQuery' action = rpcQuery action ""

heartBeat :: MonadIO m => ClientConfig -> m ()
heartBeat cfg = void $ rpcQueryIO cfg "/15/rpc" "still_alive" "" False True

clientStart :: ( MonadIO m, WithLog env Message m ) => ClientConfig -> m ()
clientStart cfg = do
    logInfo "Starting client"
    res <- parseRPCResult . getResponseBody
        <$> rpcQueryIO cfg "/15/rpc" "client_start" "" True True
    when (rpcStatusCode res /= OK) $ do
        logInfo [i|Client start failed: #{show (rpcStatusCode res)}|]
        error "Client start failed"

clientStop :: ( MonadIO m, WithLog env Message m ) => ClientConfig -> m ()
clientStop cfg = do
    logInfo "Stopping client"
    void $ rpcQueryIO cfg "/15/rpc" "client_stop" "" False False

clientLogin :: MonadIO m => ClientConfig -> m ()
clientLogin cfg = void $ rpcQueryIO cfg "/15/rpc" "client_login" "" True True

hathSettings :: ( MonadIO m, WithLog env Message m ) => ClientConfig -> m HathSettings
hathSettings cfg = do
    logInfo "Requesting client settings"
    foldl' (\s kv -> let
                ( k, rest ) = LBS.span (/= '=') kv
                v           = LBS.drop 1 rest
                in 
                    case ( k, readMaybe @Int64 (LBS.unpack v) ) of
                        ( "host", _ ) -> s { clientHost = LBS.toStrict v }
                        ( "port", Just p ) -> s { clientPort = fromIntegral p }
                        ( "throttle_bytes", Just bytes ) -> s { throttleBytes = bytes }
                        ( "disklimit_bytes", Just bytes ) -> s { diskLimitBytes = bytes }
                        ( "diskremaining_bytes", Just bytes ) -> s { diskRemainingBytes = bytes }
                        ( "static_ranges", _ ) -> s
                            { staticRanges = HashSet.fromList $ LBS.toStrict <$> LBS.split ';' v }
                        _ -> s) defaultHathSettings
        . (rpcResults . parseRPCResult . getResponseBody)
        <$> rpcQueryIO cfg "/15/rpc" "client_settings" "" True True

hathCertificate :: ( MonadIO m, WithLog env Message m ) => ClientConfig -> m LBS.ByteString
hathCertificate cfg = do
    logInfo "Requesting certificate"
    getResponseBody <$> rpcQueryIO cfg "/15/rpc" "get_cert" "" True False

fromPkcs12 :: ClientConfig -> LBS.ByteString -> ( CertificateChain, PrivKey )
fromPkcs12 (clientKey -> pass) bytes = case cred of
    Left err -> error $ show err
    Right c  -> c
  where
    cred = do
        p12 <- readP12FileFromMemory (BS.concat $ LBS.toChunks bytes)
        ( _, pkcs12 ) <- recoverAuthenticated pass p12
        maybeCred <- recover (toProtectionPassword pass) (toCredential pkcs12)
        case maybeCred of
            Just c  -> return c
            Nothing -> error "no credential"

galleryMetadata
    :: MonadIO m => ClientConfig -> Maybe ( ByteString, ByteString ) -> m (Response LBS.ByteString)
galleryMetadata cfg opt
    = rpcQueryIO
        cfg
        "/15/dl"
        "fetchqueue"
        (case opt of
             Nothing -> ""
             Just ( gid, minxres ) -> [i|#{gid};#{minxres}|])
        False
        True

galleryFetch
    :: MonadIO m => ClientConfig -> Int -> GalleryFile -> Bool -> m (Response LBS.ByteString)
galleryFetch cfg gid file forceImageServer
    = rpcQueryIO
        cfg
        "/15/rpc"
        "dlfetch"
        [i|#{gid};#{galleryFilePage file};#{galleryFileIndex file};#{galleryFileXRes file};#{fromEnum forceImageServer}|]
        False
        False
