module Metrics.Counter ( inc, add, read ) where

import           Metrics.Types

import           Relude

-- | Atomically increment counter by 1
inc :: Counter -> IO ()
inc = add 1

-- | Atomically add value to counter
add :: Int -> Counter -> IO ()
add n counter = atomicModifyIORef' (counterRef counter) $ \old -> ( old + fromIntegral n, () )

{-# INLINE add #-}

-- | Read current counter value
-- This is used for snapshot generation and should not be used in hot paths
read :: Counter -> IO Int64
read = readIORef . counterRef

{-# INLINE read #-}

-- Note: We use atomicModifyIORef' instead of modifyIORef' because:
-- 1. It's truly atomic (lock-free at CPU level)
-- 2. No MVar blocking
-- 3. Performance: ~50ns per operation vs ~500ns with MVar
