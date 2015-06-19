{-# LANGUAGE CPP, ForeignFunctionInterface #-}

{-|
Stability: stable
Portability: portable

You can generate a new CUID inside any IO-enabled monad using @newCuid@:

>>> cuid <- newCuid
>>> print cuid
"ciaafthr00000qhpm0jp81gry"

There is also a function, @newSlug@, which generates shorter IDs without the c
prefix of regular CUIDs:

>>> slug <- newSlug
>>> print slug
"ym0001iw3f"

This module does not use crypto-strength sources of randomness. Use at your own
peril!
-}
module Web.Cuid (
    Cuid, newCuid,
    Slug, newSlug
) where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (ord)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.String (fromString)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Formatting (Format, base, fitLeft, fitRight, sformat, left, (%.))
import Network.HostName (getHostName)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

import Data.Monoid (Monoid, (<>))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif

#if defined(mingw32_HOST_OS)
import System.Win32 (ProcessId, failIfZero)
#else
import System.Posix.Process (getProcessID)
#endif

-- | Convenience type so that you don't have to import Text downstream. Note that
-- this is strict Text.
type Cuid = Text

-- | Generate a new random CUID.
newCuid :: MonadIO m => m Cuid
newCuid = concatResults [c, time, count, fingerprint, random, random] where
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
        (pid, host) <- getFingerprint
        return (sformat twoOfNum pid <> sformat twoOfNum host)

    -- And some randomness for good measure. Note that System.Random is not a
    -- source of crypto-strength randomness.
    random = liftM (sformat numberPadded) (randomRIO (0, maxCount))

-- | Another convenience type. A Slug is not a Cuid.
type Slug = Text

-- | A slug is a shorter piece of text generated using some of the same
-- techniques as CUIDs.
newSlug :: MonadIO m => m Slug
newSlug = concatResults [time, count, fingerprint, random] where
    time = liftM (sformat twoOfNum . millis) getPOSIXTime
    count = liftM (sformat numberPadded) (postIncrement counter)
    random = liftM (sformat twoOfNum) (randomRIO (0, maxCount))
    fingerprint = do
        (pid, host) <- getFingerprint
        return (sformat firstOfNum pid <> sformat lastOfNum host)

-- A machine's fingerprint is derived from its PID and hostname.
getFingerprint :: IO (Int, Int)
getFingerprint = do
    pid <- getPid
    hostname <- getHostName
    let hostSum = 36 + length hostname + sum (map ord hostname)
    return (pid, hostSum)

-- Evaluate IO actions and concatenate their results.
concatResults :: (MonadIO m, Monoid a) => [IO a] -> m a
concatResults actions = liftM mconcat (liftIO $ sequence actions)

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
    incrementAndWrap count = (succ count `mod` maxCount, count)

-- These constants are to do with number formatting.
formatBase, blockSize, maxCount :: Int
formatBase = 36
blockSize = 4
maxCount = formatBase ^ blockSize

-- Number formatters for converting to the correct base and padding.
number, numberPadded, twoOfNum, firstOfNum, lastOfNum :: Format Text (Int -> Text)
number = base formatBase
numberPadded = left blockSize '0' %. number
twoOfNum = fitRight 2 %. number
lastOfNum = fitRight 1 %. number
firstOfNum = fitLeft 1 %. number

-- POSIX time library gives the result in fractional seconds.
millis :: POSIXTime -> Int
millis posix = round (posix * 1000)

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
