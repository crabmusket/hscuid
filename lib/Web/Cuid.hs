{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Web.Cuid (
    newCuid
) where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (ord)
import Data.Monoid (mconcat, (<>))
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.String (fromString)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Formatting (Format, base, fitRight, sformat, left, (%.))
import Network.HostName (getHostName)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

#if defined(mingw32_HOST_OS)
import System.Win32 (ProcessId, failIfZero)
#else
import System.Posix.Process (getProcessID)
#endif

-- | Generate a new random CUID.
newCuid :: MonadIO m => m Text
newCuid = concatM [c, time, count, fingerprint, random, random] where
    -- The CUID starts with a letter so it's usable in HTML element IDs.
    c = return (fromString "c")

    -- The second chunk is the timestamp. Note that this means it is possible
    -- to determine the time a particular CUID was created.
    time = liftM (sformat number . millis) getPOSIXTime

    -- To avoid collisions on the same machine, add a global counter to each ID.
    count = liftM (sformat numberPadded) (postIncrement counter)

    -- To avoid collisions between separate machines, generate a 'fingerprint'
    -- from details which are hopefully unique to this machine - PID and hostname.
    fingerprint = do
        pid <- getPid
        hostname <- getHostName
        let hostSum = 36 + length hostname + sum (map ord hostname)
            twoOfNum = sformat (fitRight 2 %. number)
        return (twoOfNum pid <> twoOfNum hostSum)

    -- And some randomness for good measure. Note that System.Random is not a
    -- source of crypto-strength randomness.
    random = liftM (sformat numberPadded) (randomRIO (0, maxValue))

    -- Evaluate IO actions and concatenate their results.
    concatM actions = liftM mconcat (liftIO $ sequence actions)

    -- POSIX time library gives the result in fractional seconds.
    millis posix = round (posix * 1000)

-- CUID calls for a globally incrementing counter per machine. This is ugly,
-- but it satisfies the requirement.
counter :: IORef Int
counter = unsafePerformIO (newIORef 0)
-- Don't want two different counters being created because of inlining.
-- For more info: https://wiki.haskell.org/Top_level_mutable_state
{-# NOINLINE counter #-}

-- Increment the counter, and return the value before it was incremented.
postIncrement :: MonadIO m => IORef Int -> m Int
postIncrement c = liftIO (atomicModifyIORef' c incrementAndWrap) where
    incrementAndWrap count = (succ count `mod` maxValue, count)

-- These constants are to do with number formatting.
formatBase, blockSize, maxValue :: Int
formatBase = 36
blockSize = 4
maxValue = formatBase ^ blockSize

-- Number formatters for converting to the correct base and padding.
number, numberPadded :: Format Text (Int -> Text)
(number, numberPadded) = (toBase, toBlockSize %. toBase) where
    toBlockSize = left blockSize '0'
    toBase = base formatBase

-- Get the ID of the current process. This function has a platform-specific
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
