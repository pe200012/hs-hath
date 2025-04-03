
module Hash ( hash ) where

import           Crypto.Hash ( SHA1 )
import qualified Crypto.Hash as Crypto

import           Relude

{-# INLINE hash #-}
{-# SPECIALISE hash :: ByteString -> Text #-}
{-# SPECIALISE hash :: ByteString -> ByteString #-}
-- | Hash a ByteString using SHA1 and convert to a hex string
hash :: IsString a => ByteString -> a
hash = show . Crypto.hash @ByteString @SHA1
