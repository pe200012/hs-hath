{-# LANGUAGE DataKinds #-}

-- | Finding a requested file: in the cache, or else upstream.
module Hath.Locate ( Located(..), locateResource ) where

import           Colog                   ( Message )
import           Colog.Polysemy          ( Log )

import           Control.Concurrent      ( forkIO )
import           Control.Exception       ( finally )

import qualified Data.ByteString.Char8   as BS
import qualified Data.List               as List
import           Data.String.Interpolate ( i )

import           Hath.Cache              ( Cache, CacheHit, lookupFile, storeFile )
import           Hath.Config             ( ClientConfig )
import           Hath.Error              ( HathError )
import           Hath.FileURI            ( FileURI(..) )
import           Hath.Log                ( logInfo, logWarning )
import           Hath.Protocol           ( hathRequestToken, outgoingHeaders, sha1Hex )
import           Hath.RPC                ( RPC, fileSources )
import           Hath.Stats              ( StatsEnv, addDownload, incFetched )

import qualified Network.HTTP.Client     as HC
import           Network.HTTP.Client.TLS ( getGlobalManager )
import           Network.HTTP.Types      ( hContentLength, statusCode )

import           Polysemy
import           Polysemy.Error          ( Error, catch )
import           Polysemy.Final          ( withWeavingToFinal )
import           Polysemy.Reader         ( Reader, ask )

import           Relude                  hiding ( Reader, ask )

import           UnliftIO                ( try )

data Located
  = Stored !CacheHit
    -- | A cache miss being relayed from upstream: call it with a chunk writer.
    -- It closes the upstream connection and caches the file once it is complete.
  | Streamed !((ByteString -> IO ()) -> IO ())

-- | Look in the cache; on a miss, ask the RPC server where the file is, by the
-- @fileindex@ and @xres@ of the request and the file id as the reader asked for
-- it, and start relaying it.
--
-- Like the official client, we serve any file the server sends readers here
-- for; static ranges only matter for pruning the cache.
locateResource
  :: Members
    '[ Cache
     , RPC
     , Reader StatsEnv
     , Log Message
     , Reader ClientConfig
     , Error HathError
     , Embed IO
     , Final IO
     ]
    r
  => ByteString
  -> FileURI
  -> ( ByteString, ByteString )
  -> Sem r (Maybe Located)
locateResource fileId uri ( fileIndex, xres ) = lookupFile uri >>= \case
  Just hit -> pure $ Just $ Stored hit
  Nothing  -> do
    logInfo [i|Fetching resource: #{uri}|]
    openUpstream fileId uri fileIndex xres >>= \case
      Just response -> do
        incFetched
        -- runs after the handler has returned, once the body has been relayed
        cacheIt <- deferToIO $ \content -> do
          addDownload (BS.length content)
          -- a failed cache write only costs us the next hit
          catch @HathError (storeFile uri content)
            $ \e -> logWarning [i|Cache write failed for #{uri}: #{e}|]
        pure $ Just $ Streamed $ streamUpstream uri response (void . forkIO . cacheIt)
      Nothing       -> do
        logWarning "Failed to fetch resource, sorry for you"
        pure Nothing

-- | Open the first source that answers 200 with the expected length, as the
-- official ProxyFileDownloader does. The caller reads the body and must close
-- the response.
openUpstream :: Members '[ RPC, Reader ClientConfig, Log Message, Error HathError, Embed IO ] r
             => ByteString
             -> FileURI
             -> ByteString
             -> ByteString
             -> Sem r (Maybe (HC.Response HC.BodyReader))
openUpstream fileId uri fileIndex xres = do
  -- without sources the reader gets a 404, as from the official client
  urls <- fileSources fileIndex xres fileId `catch` \(e :: HathError)
    -> [] <$ logWarning [i|srfetch failed for #{uri}: #{e}|]
  cfg <- ask @ClientConfig
  let headers          = outgoingHeaders [ ( "Hath-Request", hathRequestToken cfg fileId ) ]
      open manager url = case HC.parseRequest url of
        Nothing  -> pure Nothing
        Just req -> try
          @IO
          @HC.HttpException
          (HC.responseOpen req { HC.requestHeaders = headers } manager)
          >>= \case
            Right response
              | statusCode (HC.responseStatus response) == 200
              , List.lookup hContentLength (HC.responseHeaders response)
                == Just (show (fileSize uri)) -> pure (Just response)
              | otherwise -> HC.responseClose response $> Nothing
            -- connection error: try the next source
            Left _         -> pure Nothing
  embed $ do
    manager <- getGlobalManager
    runMaybeT (asum (MaybeT . open manager . BS.unpack <$> urls))

-- | Relay an upstream body chunk by chunk. If it arrives complete and its SHA-1
-- matches, hand the bytes to the continuation (the cache write). The upstream
-- connection is closed in every case, including the client going away.
streamUpstream :: FileURI
               -> HC.Response HC.BodyReader
               -> (ByteString -> IO ())
               -> (ByteString -> IO ())
               -> IO ()
streamUpstream uri response whenComplete write = go [] `finally` HC.responseClose response
  where
    go chunks = HC.brRead (HC.responseBody response) >>= \chunk -> if BS.null chunk
      then let
          content = BS.concat (reverse chunks)
        in 
          when (BS.length content == fileSize uri && sha1Hex content == fileHash uri)
          $ whenComplete content
      else write chunk >> go (chunk : chunks)

-- | Lower a Sem action into IO so it can run after the request handler has returned.
deferToIO :: Member (Final IO) r => (x -> Sem r ()) -> Sem r (x -> IO ())
deferToIO k = withWeavingToFinal $ \s lower _ -> pure ((\x -> void (lower (k x <$ s))) <$ s)
