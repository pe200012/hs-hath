
module SpeedTest ( bufferSending ) where

import qualified Data.ByteString       as BS

import           Relude

import qualified Servant.Types.SourceT as Source
import           Servant.Types.SourceT ( StepT )

bufferSending :: Int -> StepT m ByteString
bufferSending 0 = Source.Stop
bufferSending n = Source.Yield (BS.take m preallocated) (bufferSending (n - m))
  where
    m = min n tcpBufferSize

tcpBufferSize :: Int
tcpBufferSize = 1460

preallocated :: ByteString
preallocated = BS.pack (replicate tcpBufferSize 54)
