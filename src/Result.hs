
module Result ( module Result ) where

import           Data.ByteString.Lazy.Char8 ( ByteString, split )

import           Relude                     hiding ( ByteString )

data StatusCode = OK | INVALID_REQUEST | KEY_EXPIRED | Other {-# UNPACK #-} !ByteString
    deriving ( Show, Eq )

data RPCResult
    = RPCResult
    { rpcStatusCode :: {-# UNPACK #-} !StatusCode, rpcResults :: {-# UNPACK #-} ![ ByteString ] }
    deriving ( Show )

parseRPCResult :: ByteString -> RPCResult
parseRPCResult bytes = case status of
    [] -> RPCResult (Other "Empty") results
    [ "OK" ] -> RPCResult OK results
    [ "INVALID_REQUEST" ] -> RPCResult INVALID_REQUEST results
    [ "KEY_EXPIRED" ] -> RPCResult KEY_EXPIRED results
    (code : _) -> RPCResult (Other code) results
  where
    segments = split '\n' bytes

    ( status, results ) = splitAt 1 segments
