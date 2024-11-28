
module Utils ( hathHash ) where

import           Crypto.Hash ( SHA1, hash )

import           Relude

-- | Hash a ByteString using SHA1 and convert to a hex string
hathHash :: ByteString -> ByteString
hathHash = show . hash @ByteString @SHA1
