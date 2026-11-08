module Metrics.Histogram ( observe ) where

import           Metrics.Types

import           Relude

-- | Record one observation.
observe :: Double -> Histogram -> IO ()
observe v h = do
  let bucket = length (takeWhile (< v) (histogramBounds h))
  whenJust (histogramCounts h !!? bucket) $ \ref -> atomicModifyIORef' ref $ \n -> ( n + 1, () )
  atomicModifyIORef' (histogramSum h) $ \s -> ( s + v, () )
