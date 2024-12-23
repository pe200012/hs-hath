{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Genesis ( Genesis(..), fetchSettings, fetchCertificate, runGenesis, runGenesisIO ) where

import           API

import           Data.X509          ( CertificateChain, PrivKey )

import           Polysemy
import           Polysemy.Error     ( Error, errorToIOFinal )
import           Polysemy.Operators
import           Polysemy.Reader    ( Reader, runReader )

import           Relude             hiding ( Reader, runReader )

import           Servant.Client     ( ClientError )

import           Types

data Genesis m a where
    FetchSettings :: Genesis m HathSettings
    FetchCertificate :: Genesis m ( CertificateChain, PrivKey )

makeSem ''Genesis

{-# INLINE runGenesis #-}
runGenesis :: Members '[ Embed IO, Error RPCError, Reader ClientConfig, EHentaiAPI ] r
           => Genesis : r @> a
           -> Sem r a
runGenesis = interpret $ \case
    FetchSettings    -> getSettings
    FetchCertificate -> downloadCertificates

{-# INLINE runGenesisIO #-}
runGenesisIO :: ClientConfig
             -> [ Genesis
                , EHentaiAPI
                , Reader ClientConfig
                , Error ClientError
                , Error RPCError
                , Embed IO
                , Final IO
                ] @> a
             -> IO (Either RPCError (Either ClientError a))
runGenesisIO cfg
    = runFinal
    . embedToFinal @IO
    . errorToIOFinal @RPCError
    . errorToIOFinal @ClientError
    . runReader cfg
    . runEHentaiAPI
    . runGenesis
