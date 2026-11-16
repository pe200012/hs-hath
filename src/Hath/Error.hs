-- | What can go wrong between this client, the RPC server and the cache.
module Hath.Error ( HathError(..) ) where

import           Relude

import           Servant.Client.Core ( ClientError )

data HathError
  = TransportError !ClientError
    -- ^ The HTTP exchange with the RPC server failed.
  | EmptyResponse
    -- ^ The RPC server sent nothing.
  | RequestFailure !Text
    -- ^ The RPC server answered with this status instead of @OK@.
  | CertificateFailure !Text
    -- ^ The PKCS#12 file from @get_cert@ could not be read.
  | StorageError !Text
    -- ^ A cache write failed.
  deriving ( Show, Eq )

instance Exception HathError
