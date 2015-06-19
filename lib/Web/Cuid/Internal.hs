{-# LANGUAGE CPP, ForeignFunctionInterface #-}

{-|
Stability: unstable
Portability: portable

Contains internal implementation details for CUIDs.
-}
module Web.Cuid.Internal where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (ord)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Formatting (Format, base, fitLeft, fitRight, left, (%.))
import Network.HostName (getHostName)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

#if defined(mingw32_HOST_OS)
import System.Win32 (ProcessId, failIfZero)
#else
import System.Posix.Process (getProcessID)
#endif

-- | A machine's fingerprint is derived from its PID and hostname. We do some
-- maths on the hostname's contents to boil it down to a single integer instead
-- of exposing a string.
getFingerprint :: IO (Int, Int)
getFingerprint = do
    pid <- getPid
    hostname <- getHostName
    let hostSum = 36 + length hostname + sum (map ord hostname)
    return (pid, hostSum)

-- | Just get a random integer. Not referentially transparent.
getRandomValue :: IO Int
getRandomValue = randomRIO (0, maxCount)

-- | CUID calls for a globally incrementing counter per machine. This is ugly,
-- but it satisfies the requirement.
counter :: IORef Int
counter = unsafePerformIO (newIORef 0)
-- Don't want two different counters being created because of inlining.
-- For more info: https://wiki.haskell.org/Top_level_mutable_state
{-# NOINLINE counter #-}

-- | Get the next value of the global counter required for CUID.
getNextCount :: IO Int
getNextCount = postIncrement counter

-- | Increment the counter, and return the value before it was incremented.
postIncrement :: MonadIO m => IORef Int -> m Int
postIncrement c = liftIO (atomicModifyIORef' c incrementAndWrap) where
    incrementAndWrap count = (succ count `mod` maxCount, count)

-- | These constants are to do with number formatting.
formatBase, blockSize, maxCount :: Int
formatBase = 36
blockSize = 4
maxCount = formatBase ^ blockSize

-- | Number formatters for converting to the correct base and padding.
number, numberPadded, twoOfNum, firstOfNum, lastOfNum :: Format Text (Int -> Text)
number = base formatBase
numberPadded = left blockSize '0' %. number
twoOfNum = fitRight 2 %. number
lastOfNum = fitRight 1 %. number
firstOfNum = fitLeft 1 %. number

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
