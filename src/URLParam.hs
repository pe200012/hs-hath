
module URLParam ( URLParams, parseURLParams, lookupParam, hasParam ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as Map

import           Relude

type URLParams = Map.Map ByteString ByteString

{-# INLINE parseURLParams #-}
-- | Parse URL parameters in the format "key1=value1;key2=value2;key3;key4"
--
-- >>> parseURLParams "key1=value1;key2=value2;key3;key4"
parseURLParams :: ByteString -> URLParams
parseURLParams = foldl' insertPair Map.empty . BS.split ';'
  where
    insertPair acc pair = case BS.split '=' pair of
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
