module Web.Cuid (
    newCuid
) where

import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.String (fromString)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

newCuid :: IO Text
newCuid = return (fromString "c")

type Counter = IORef Int

counter :: Counter
counter = unsafePerformIO (newIORef 0)

next :: Counter -> IO Int
next c = atomicModifyIORef' c incrementAndWrap where
    incrementAndWrap count = (succ count `mod` countMax, count)
    countMax = base ^ blockSize
    blockSize = 4
    base = 36
