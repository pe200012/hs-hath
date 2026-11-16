-- | The RPC server's clock. Keystamps, server commands, speed tests and the
-- signatures of our own RPC requests all carry its time, so we sign and check
-- with it rather than ours (official Settings.getServerTime). As the official
-- client does, we keep its offset from our clock in one global: it is read
-- wherever a time is signed or checked, and set from whichever reply carries
-- @server_time@.
module Hath.ServerTime ( serverNow, setServerTime ) where

import           Data.Time.Clock.System ( SystemTime(systemSeconds), getSystemTime )

import           Relude

import           System.IO.Unsafe       ( unsafePerformIO )

-- | Seconds to add to our clock to get the server's.
offset :: IORef Int64
offset = unsafePerformIO (newIORef 0)

{-# NOINLINE offset #-}

-- | The server's time in Unix seconds: ours until a reply has told us its own.
serverNow :: IO Int64
serverNow = (+) <$> readIORef offset <*> ourNow

-- | Take the server's clock from the @server_time@ it sent.
setServerTime :: Int64 -> IO ()
setServerTime serverTime = ourNow >>= atomicWriteIORef offset . (serverTime -)

ourNow :: IO Int64
ourNow = systemSeconds <$> getSystemTime
