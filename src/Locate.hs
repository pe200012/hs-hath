{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Locate ( LocateURI(..), Locate(..), locateResource, runLocate ) where

import           API                   ( EHentaiAPI, fetchResource )

import qualified Data.ByteString.Short as SBS
import qualified Data.HashSet          as HashSet
import qualified Data.Map              as Map

import           Database              ( FileRecord(..) )

import           Polysemy
import           Polysemy.Error        ( Error )
import           Polysemy.KVStore      ( KVStore, lookupKV, updateKV )
import           Polysemy.Reader       ( Reader, ask )

import           Relude                hiding ( Reader, ask )

import           Types

data LocateURI
    = LocateURI { locateURIFilename :: !ByteString
                , locateURI         :: !FileURI
                , locateURIOptions  :: !(Map ByteString ByteString)
                }

data Locate m a where
    -- | Fetch a specific resource from the server
    LocateResource :: LocateURI -> Locate m (Maybe ByteString)

makeSem ''Locate

runLocate :: Members
              '[ KVStore FileURI FileRecord
               , Reader HathSettings
               , Error RPCError
               , Reader ClientConfig
               , EHentaiAPI
               , Embed IO
               ]
              r
          => Sem (Locate ': r) a
          -> Sem r a
runLocate = interpret $ \case
    LocateResource uri -> do
        settings <- ask @HathSettings
        let fileURI = locateURI uri
            s4      = SBS.take 4 . show $ fileURI
        if HashSet.member s4 (staticRanges settings)
            then lookupKV fileURI >>= \case
                Just record -> return $ Just $ fileRecordBytes record
                Nothing     -> case ( Map.lookup "fileindex" (locateURIOptions uri)
                                    , Map.lookup "xres" (locateURIOptions uri)
                                    ) of
                    ( Just fileIndex, Just xres ) -> fetchResource fileURI ( fileIndex, xres ) >>= \case
                        Just content -> do
                            updateKV
                                fileURI
                                (Just
                                 $ FileRecord
                                 { fileRecordLRUCounter = 1
                                 , fileRecordS4         = decodeUtf8 s4
                                 , fileRecordFileId     = show fileURI
                                 , fileRecordFileName   = Just $ decodeUtf8 $ locateURIFilename uri
                                 , fileRecordBytes      = content
                                 })
                            pure $ Just content
                        Nothing      -> pure Nothing
                    _ -> pure Nothing
            else pure Nothing


