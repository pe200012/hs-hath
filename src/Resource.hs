{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Resource ( module Resource ) where

import           Cache

import           Colog                      ( logInfo )

import           Control.Applicative        ( asum )
import           Control.Monad.Catch        ( MonadThrow )
import           Control.Monad.IO.Class     ( MonadIO )
import           Control.Monad.Reader       ( asks )

import           Data.ByteString            ( ByteString )
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashSet               as HashSet
import qualified Data.Map                   as Map
import           Data.String.Interpolate    ( i )

import           Network.HTTP.Client
import           Network.HTTP.Simple        ( getResponseBody, getResponseStatus )
import qualified Network.HTTP.Simple        as Simple
import           Network.HTTP.Types         ( status200 )

import           Query

import           Relude

import           Result                     ( RPCResult(rpcResults), parseRPCResult )

import           Types

import           Utils

locateResource :: ( MonadIO m, MonadThrow m )
               => ByteString
               -> HathFile
               -> Map ByteString ByteString
               -> HathM m (Maybe ByteString)
locateResource filename fileid opts = do
    sr <- staticRanges <$> getHathSettings
    if inStaticRange fileid sr
        then do
            logInfo [i|Locating resource for #{fileid}|]
            lookupCache rawFileId >>= \case
                Just bytes -> do
                    updateFilenameIfMissing rawFileId filename
                    return $ Just bytes
                Nothing    -> case ( Map.lookup "fileindex" opts, Map.lookup "xres" opts ) of
                    ( Just fi, Just xres ) -> do
                        cid <- asks (clientID . clientConfig)
                        ckey <- asks (clientKey . clientConfig)
                        srcs <- fetchResource fi xres rawFileId
                        mapM (pullResource (reqToken cid ckey)) srcs
                            >>= maybe (return Nothing) (\bytes -> do
                                                            storeCache fileid bytes filename
                                                            return $ Just bytes) . asum
                    _ -> return Nothing
        else return Nothing
  where
    rawFileId = toFileId fileid

    reqToken :: ByteString -> ByteString -> ByteString
    reqToken cid ckey = [i|#{cid}-#{hathHash (ckey <> rawFileId)}|]

    inStaticRange p = HashSet.member (BS.take 4 (fileHash p))

pullResource
    :: ( MonadIO m, MonadThrow m ) => ByteString -> LBS.ByteString -> HathM m (Maybe ByteString)
pullResource reqToken uri = do
    initReq <- parseRequest $ LBS.unpack uri
    let req
            = setRequestIgnoreStatus
            $ initReq
            { method = "GET", requestHeaders = ( "Hath-Request", reqToken ) : commonHeader }
    res <- Simple.httpLbs req
    return
        $ if getResponseStatus res == status200
            then Just $ LBS.toStrict $ getResponseBody res
            else Nothing

fetchResource :: MonadIO m => ByteString -> ByteString -> ByteString -> HathM m [ LBS.ByteString ]
fetchResource fileindex xres fileid = do
    logInfo [i|Fetching resource #{fileid}|]
    filter (not . LBS.null) . rpcResults . parseRPCResult . getResponseBody
        <$> rpcQuery "srfetch" [i|#{fileindex};#{xres};#{fileid}|] False False

