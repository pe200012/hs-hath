
module Utils ( module Utils ) where

import           Colog                 ( Message, Msg(..), Severity )
import           Colog.Polysemy        ( Log )
import qualified Colog.Polysemy        as Co

import           Crypto.Hash           ( SHA1 )
import qualified Crypto.Hash           as Crypto

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict       as Map

import           Polysemy.Operators

import           Relude

import qualified Servant.Types.SourceT as Source
import           Servant.Types.SourceT ( StepT )

type URLParams = Map.Map ByteString ByteString

{-# INLINE parseURLParams #-}
-- | Parse URL parameters in the format "key1=value1;key2=value2;key3;key4"
--
-- >>> parseURLParams "key1=value1;key2=value2;key3;key4"
parseURLParams :: ByteString -> URLParams
parseURLParams = foldl' insertPair Map.empty . BS8.split ';'
  where
    insertPair acc pair = case BS8.split '=' pair of
      []           -> acc
      [ key ]      -> Map.insert key mempty acc  -- Parameter without value becomes empty string
      [ key, val ] -> Map.insert key val acc
      key : val    -> error $ "Invalid URL parameter: " <> show key <> "=" <> show val

{-# INLINE lookupParam #-}
-- | Lookup a parameter value
lookupParam :: ByteString -> URLParams -> Maybe ByteString
lookupParam = Map.lookup

{-# INLINE hasParam #-}
-- | Check if a parameter exists
hasParam :: ByteString -> URLParams -> Bool
hasParam = Map.member

bufferSending :: Int -> StepT m ByteString
bufferSending 0 = Source.Stop
bufferSending n
  | n >= tcpBufferSize = Source.Yield preallocated (bufferSending (n - tcpBufferSize))
  | otherwise = Source.Yield (BS.take n preallocated) Source.Stop
  where
    tcpBufferSize :: Int
    tcpBufferSize = 1460

    preallocated :: ByteString
    preallocated = BS.replicate tcpBufferSize 54

-- msg :: sev -> Text -> Msg sev
-- msg sev m = withFrozenCallStack (Msg { msgSeverity = sev, msgStack = callStack, msgText = m })
{-# INLINE log #-}
log :: Severity -> Text -> Log Message -@> ()
log sev msg
  = withFrozenCallStack (Co.log Msg { msgSeverity = sev, msgStack = callStack, msgText = msg })

{-# INLINE hash #-}
{-# SPECIALISE hash :: ByteString -> Text #-}
{-# SPECIALISE hash :: ByteString -> ByteString #-}
-- | Hash a ByteString using SHA1 and convert to a hex string
hash :: IsString a => ByteString -> a
hash = show . Crypto.hash @ByteString @SHA1
