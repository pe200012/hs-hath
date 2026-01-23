module Metrics.Gauge ( inc, dec, set, add, sub, read ) where

import           Metrics.Types

import           Relude

-- | Atomically increment gauge by 1
inc :: Gauge -> IO ()
inc = add 1

{-# INLINE inc #-}

-- | Atomically decrement gauge by 1
dec :: Gauge -> IO ()
dec = sub 1

{-# INLINE dec #-}

-- | Set gauge to exact value (non-atomic write is sufficient for gauges)
set :: Int64 -> Gauge -> IO ()
set n gauge = writeIORef (gaugeRef gauge) n

{-# INLINE set #-}

-- | Atomically add value to gauge
add :: Int -> Gauge -> IO ()
add n gauge = atomicModifyIORef' (gaugeRef gauge) $ \old -> ( old + fromIntegral n, () )

{-# INLINE add #-}

-- | Atomically subtract value from gauge
sub :: Int -> Gauge -> IO ()
sub n = add (negate n)

{-# INLINE sub #-}

-- | Read current gauge value
read :: Gauge -> IO Int64
read = readIORef . gaugeRef

{-# INLINE read #-}
