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

foreign import stdcall unsafe "windows.h GetCurrentProcessId"
    c_GetCurrentProcessId :: IO ProcessId

getCurrentProcessId :: IO ProcessId
getCurrentProcessId = failIfZero "GetCurrentProcessId" $ c_GetCurrentProcessId

getPid :: IO Int
getPid = fmap fromIntegral getCurrentProcessId

#else

import System.Posix.Process (getProcessID)
getPid :: IO Int
getPid = fmap fromIntegral getProcessID

#endif

newCuid :: IO Text
newCuid = concatM [c, time, count, fingerprint, random, random] where
    concatM actions = fmap mconcat (sequence actions)
    c = return $ fromString "c"
    time = fmap (format number . millis) getPOSIXTime
    count = fmap (format numberPadded) (next counter)
    fingerprint = fmap (format numberPadded) getPid
    random = fmap (format numberPadded) (randomRIO (0, maxValue))
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
