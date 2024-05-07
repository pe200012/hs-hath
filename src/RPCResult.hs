{-# LANGUAGE OverloadedStrings #-}

module RPCResult ( module RPCResult ) where

import           Data.ByteString.Lazy.Char8 ( ByteString, split )

data StatusCode = OK | Other ByteString
    deriving ( Show )

data RPCResult = RPCResult { rpcStatusCode :: StatusCode, rpcResults :: [ ByteString ] }
    deriving ( Show )

parseRPCResult :: ByteString -> RPCResult
parseRPCResult bytes = case status of
    []       -> RPCResult (Other "Empty") results
    [ "OK" ] -> RPCResult OK results
    _        -> RPCResult (Other $ head status) results
  where
    segments = split '\n' bytes

    ( status, results ) = splitAt 1 segments
