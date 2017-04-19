{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns #-}

{-|
Stability: unstable
Portability: portable

Contains internal implementation details for CUIDs.
-}
module Web.Cuid.Internal (
    formatNumber, formatPadded, formatShort,
    getNextCount, getRandomValue, getTimestamp, myFingerprint
) where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (ord)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Text (Text, append)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HostName (getHostName)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (GenIO)
import qualified System.Random.MWC as MWC
import Web.Cuid.Internal.Formatting

#if defined(mingw32_HOST_OS)
import System.Win32 (ProcessId, failIfZero)
#else
import System.Posix.Process (getProcessID)
#endif

-- | maxCount is derived from the output format and defines the maximum random
-- number we should generate.
maxCount :: Int
maxCount = formatBase ^ blockSize

-- | A machine's fingerprint is derived from its PID and hostname. We do some
-- maths on the hostname's contents to boil it down to a single integer instead
-- of exposing a string.
getFingerprint :: IO (Int, Int)
getFingerprint = do
    pid <- getPid
    hostname <- getHostName
    let hostSum = 36 + length hostname + sum (map ord hostname)
    return (pid, hostSum)

-- | For efficiency, calculate the fingerptint and format it once.
myFingerprint :: Text
myFingerprint = unsafePerformIO $ do
    (pid, host) <- getFingerprint
    return (formatShort pid `append` formatShort host)
-- This ensures the action should only be evaluated once, rather than being
-- inlined and potentially evaluated inside another call.
{-# NOINLINE myFingerprint #-}

-- | Global random number generator.
generator :: GenIO
generator = unsafePerformIO MWC.create
-- Don't want two different generators being created because of inlining.
-- For more info: https://wiki.haskell.org/Top_level_mutable_state
{-# NOINLINE generator #-}

-- | Just get a random integer.
getRandomValue :: IO Int
getRandomValue = MWC.uniformR (0, maxCount) generator

-- | CUID calls for a globally incrementing counter per machine. This is ugly,
-- but it satisfies the requirement.
counter :: IORef Int
counter = unsafePerformIO (newIORef 0)
-- Don't want two different counters being created because of inlining.
{-# NOINLINE counter #-}

-- | Get the next value of the global counter required for CUID.
getNextCount :: IO Int
getNextCount = postIncrement counter

-- | Increment the counter, and return the value before it was incremented.
postIncrement :: MonadIO m => IORef Int -> m Int
postIncrement c = liftIO (atomicModifyIORef' c incrementAndWrap) where
    incrementAndWrap count = (succ count `mod` maxCount, count)

-- | Get the current UNIX time in milliseconds.
getTimestamp :: IO Int
getTimestamp = liftM toMillis getPOSIXTime where
    toMillis posix = round (posix * 1000)

-- | Get the ID of the current process. This function has a platform-specific
-- implementation. Fun times.
getPid :: MonadIO m => m Int

#if defined(mingw32_HOST_OS)

foreign import stdcall unsafe "windows.h GetCurrentProcessId"
    c_GetCurrentProcessId :: IO ProcessId

getCurrentProcessId :: IO ProcessId
getCurrentProcessId = failIfZero "GetCurrentProcessId" c_GetCurrentProcessId

getPid = liftM fromIntegral (liftIO getCurrentProcessId)

#else

getPid = liftM fromIntegral (liftIO getProcessID)

#endif
