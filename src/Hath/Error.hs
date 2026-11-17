-- | What can go wrong between this client, the RPC server and the cache.
module Hath.Error ( HathError(..) ) where

import           Relude

data HathError
  = TransportError !Text
    -- ^ The HTTP exchange with the RPC server failed, every time it was tried.
  | EmptyResponse
  | RequestFailure !Text
    -- ^ The RPC server answered with this status instead of @OK@.
  | CertificateFailure !Text
    -- ^ The PKCS#12 file from @get_cert@ could not be read.
  | StorageError !Text
    -- ^ A cache write failed.
  deriving ( Show, Eq )

instance Exception HathError
