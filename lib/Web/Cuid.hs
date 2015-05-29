{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Web.Cuid (
    newCuid
) where

import Data.Monoid (mconcat)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.String (fromString)
import Data.Text.Lazy (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Formatting (Format, base, format, left, (%.))
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

#if defined(mingw32_HOST_OS)
import System.Win32 (ProcessId, failIfZero)
#else
import System.Posix.Process (getProcessID)
#endif

-- | Generate a new random CUID.
newCuid :: IO Text
newCuid = concatM [c, time, count, fingerprint, random, random] where
    -- The CUID starts with a letter so it's usable in HTML element IDs.
    c = return $ fromString "c"
    -- The second chunk is the timestamp. Note that this means it is possible
    -- to determine the time a particular CUID was created.
    time = fmap (format number . millis) getPOSIXTime
    -- To avoid collisions on the same machine, add a global counter to each ID.
    count = fmap (format numberPadded) (next counter)
    -- To avoid collosions between separate machines, generate a 'fingerprint'
    -- from details which are hopefully unique to this machine - PID and hostname.
    fingerprint = fmap (format numberPadded) getPid
    -- And some randomness for good measure.
    random = fmap (format numberPadded) (randomRIO (0, maxValue))

    -- Evaluate IO actions and concatenate their results.
    concatM actions = fmap mconcat (sequence actions)
    -- POSIX time library gives the result in fractional seconds.
    millis posix = round $ posix * 1000

type Counter = IORef Int

counter :: Counter
counter = unsafePerformIO (newIORef 0)

next :: Counter -> IO Int
next c = atomicModifyIORef' c incrementAndWrap where
    incrementAndWrap count = (succ count `mod` maxValue, count)

formatBase, blockSize, maxValue :: Int
formatBase = 36
blockSize = 4
maxValue = formatBase ^ blockSize

number, numberPadded :: Format Text (Int -> Text)
(number, numberPadded) = (toBase, toBlockSize %. toBase) where
    toBlockSize = left blockSize '0'
    toBase = base formatBase

getPid :: IO Int

#if defined(mingw32_HOST_OS)

foreign import stdcall unsafe "windows.h GetCurrentProcessId"
    c_GetCurrentProcessId :: IO ProcessId

getCurrentProcessId :: IO ProcessId
getCurrentProcessId = failIfZero "GetCurrentProcessId" $ c_GetCurrentProcessId

getPid = fmap fromIntegral getCurrentProcessId

#else

getPid = fmap fromIntegral getProcessID

#endif
