{-# LANGUAGE DeriveGeneric #-}

module RPC ( RPCResponse(..), RPCError(..), parseRPCResponse, getPayload ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS

import           Relude

data RPCResponse = RPCResponse { statusCode :: !ByteString, payload :: ![ ByteString ] }
    deriving ( Show, Eq, Generic )

data RPCError = EmptyResponse | RequestFailure ByteString  -- Contains the error status code
    deriving ( Show, Eq, Generic )

instance Exception RPCError

-- | Parse an RPC response from a lazy ByteString
-- The first line is the status code, followed by the payload lines
parseRPCResponse :: LBS.ByteString -> Either RPCError RPCResponse
parseRPCResponse input = case BS.lines (LBS.toStrict input) of
    [] -> Left EmptyResponse
    status : rest -> Right $ RPCResponse { statusCode = status, payload = rest }

-- | Get the payload if the response was successful
getPayload :: RPCResponse -> Either RPCError [ ByteString ]
getPayload response
    | statusCode response == "OK" = Right $ payload response
    | otherwise = Left $ RequestFailure $ statusCode response