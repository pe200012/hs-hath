{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Locate ( LocateURI(..), Locate(..), locateResource, runLocate ) where

import           Colog                   ( Message, Severity(Info, Warning) )
import           Colog.Polysemy          ( Log )

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Short   as SBS
import qualified Data.HashSet            as HashSet
import qualified Data.Map                as Map
import           Data.String.Interpolate ( i )

import           Interface.API           ( EHentaiAPI, fetchResource )

import           Polysemy
import           Polysemy.Error          ( Error )
import           Polysemy.KVStore        ( KVStore, lookupKV, updateKV )
import           Polysemy.Operators
import           Polysemy.Reader         ( Reader, ask )

import           Relude                  hiding ( Reader, ask )

import           SettingM                ( SettingM, getSettings )

import           Stats                   ( Stats, addDownload, incFetched )

import           Types

import           Utils                   ( log )

data LocateURI
  = LocateURI { locateURIFilename :: !ByteString
              , locateURI         :: !FileURI
              , locateURIOptions  :: !(Map ByteString ByteString)
              }

data Locate m a where
  -- | Fetch a specific resource from the server
  LocateResource :: LocateURI -> Locate m (Maybe ByteString)

makeSem ''Locate

runLocate
  :: Members
    '[ KVStore FileURI FileRecord
     , SettingM
     , Error RPCError
     , Reader ClientConfig
     , EHentaiAPI
     , Embed IO
     , Stats
     , Log Message
     ]
    r
  => Locate : r @> a
  -> r @> a
runLocate = interpret $ \case
  LocateResource uri -> do
    settings <- getSettings
    let fileURI = locateURI uri
        s4      = SBS.take 4 . show $ fileURI
    if HashSet.member s4 (staticRanges settings)
      then lookupKV fileURI >>= \case
        Just record -> return $ Just $ fileRecordBytes record
        Nothing     -> case ( Map.lookup "fileindex" (locateURIOptions uri)
                            , Map.lookup "xres" (locateURIOptions uri)
                            ) of
          ( Just fileIndex, Just xres ) -> log Info [i|Fetching resource: #{fileURI}|]
            >> fetchResource fileURI ( fileIndex, xres )
            >>= \case
              Just content -> do
                incFetched
                addDownload (BS.length content)
                updateKV fileURI
                  $ Just
                  $ FileRecord { fileRecordLRUCounter = 1
                               , fileRecordS4         = decodeUtf8 s4
                               , fileRecordFileId     = show fileURI
                               , fileRecordFileName   = Just $ decodeUtf8 $ locateURIFilename uri
                               , fileRecordBytes      = content
                               }
                pure $ Just content
              Nothing      -> do
                log Warning [i|Failed to fetch resource, sorry for you|]
                pure Nothing
          _ -> do
            log Info [i|Not enough information to fetch resource: #{fileURI}|]
            pure Nothing
      else do
        log Info [i|Resource not in our ranges, rejecting: #{s4}|]
        pure Nothing
