{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Locate
  ( LocateURI(..)
  , Locate(..)
  , Located(..)
  , locateResource
  , runLocate
  , streamUpstream
  ) where

import           Colog                   ( Message, Severity(Info, Warning) )
import           Colog.Polysemy          ( Log )

import           Control.Concurrent      ( forkIO )
import           Control.Exception       ( finally )

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Short   as SBS
import qualified Data.Map                as Map
import           Data.String.Interpolate ( i )

import           Interface.API           ( EHentaiAPI, openResource )

import qualified Network.HTTP.Client     as HC

import           Polysemy
import           Polysemy.Error          ( Error, catch )
import           Polysemy.Final          ( withWeavingToFinal )
import           Polysemy.KVStore        ( KVStore, lookupKV, updateKV )
import           Polysemy.Operators
import           Polysemy.Reader         ( Reader )

import           Relude                  hiding ( Reader, ask )

import           Stats                   ( Stats, addDownload, incFetched )

import           Types

import           Utils                   ( hash, log )

data LocateURI
  = LocateURI { locateURIFilename :: !ByteString
              , locateURI         :: !FileURI
              , locateURIOptions  :: !(Map ByteString ByteString)
              }

data Located
  = Stored !StorageResult
    -- | Cache miss being relayed from upstream: call it with a chunk writer.
    -- It closes the upstream connection and caches the file once it is complete.
  | Streamed !((ByteString -> IO ()) -> IO ())

data Locate m a where
  -- | Fetch a specific resource from the server
  LocateResource :: LocateURI -> Locate m (Maybe Located)

makeSem ''Locate

runLocate
  :: Members
    '[ KVStore FileURI StorageResult
     , Error RPCError
     , Reader ClientConfig
     , EHentaiAPI
     , Embed IO
     , Final IO
     , Stats
     , Log Message
     ]
    r
  => Locate : r @> a
  -> r @> a
runLocate = interpret $ \case
  LocateResource uri -> do
    let fileURI = locateURI uri
        s4      = SBS.toShort $ BS.take 4 $ renderFileURIBytes fileURI
        record content
          = Record
            FileRecord
            { fileRecordLRUCounter = 1
            , fileRecordS4         = decodeUtf8 s4
            , fileRecordFileId     = renderFileURIText fileURI
            , fileRecordFileName   = Just $ decodeUtf8 $ locateURIFilename uri
            , fileRecordBytes      = content
            }
    -- like the official client, serve any file the server sends us here; static ranges
    -- only matter for pruning the cache
    lookupKV fileURI >>= \case
      Just x -> return $ Just $ Stored x
      Nothing -> case ( Map.lookup "fileindex" (locateURIOptions uri)
                      , Map.lookup "xres" (locateURIOptions uri)
                      ) of
        ( Just fileIndex, Just xres ) -> log Info [i|Fetching resource: #{fileURI}|]
          >> openResource fileURI ( fileIndex, xres )
          >>= \case
            Just resp -> do
              incFetched
              -- runs after the handler has returned, once the body has been relayed
              cacheIt <- deferToIO $ \content -> do
                addDownload (BS.length content)
                -- a failed cache write only costs us the next hit
                catch @RPCError (updateKV fileURI $ Just $ record content)
                  $ \e -> log Warning [i|Cache write failed for #{fileURI}: #{e}|]
              pure $ Just $ Streamed $ streamUpstream fileURI resp (void . forkIO . cacheIt)
            Nothing   -> do
              log Warning [i|Failed to fetch resource, sorry for you|]
              pure Nothing
        _ -> do
          log Info [i|Not enough information to fetch resource: #{fileURI}|]
          pure Nothing

-- | Relay an upstream body chunk by chunk. If it arrives complete and its SHA-1
-- matches, hand the bytes to the continuation (the cache write). The upstream
-- connection is closed in every case, including the client going away.
streamUpstream :: FileURI
               -> HC.Response HC.BodyReader
               -> (ByteString -> IO ())
               -> (ByteString -> IO ())
               -> IO ()
streamUpstream fileURI resp whenComplete write = go [] `finally` HC.responseClose resp
  where
    go acc = HC.brRead (HC.responseBody resp) >>= \chunk -> if BS.null chunk
      then let
          content = BS.concat (reverse acc)
        in
          when (BS.length content == fileSize fileURI && hash content == fileHash fileURI)
            $ whenComplete content
      else write chunk >> go (chunk : acc)

-- | Lower a Sem action into IO so it can run after the request handler has returned.
deferToIO :: Member (Final IO) r => (x -> Sem r ()) -> Sem r (x -> IO ())
deferToIO k = withWeavingToFinal $ \s lower _ -> pure ((\x -> void (lower (k x <$ s))) <$ s)
