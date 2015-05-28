module Web.Cuid (
    newCuid
) where

import Data.Monoid (mconcat)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.String (fromString)
import Data.Text.Lazy (Text)
import System.IO.Unsafe (unsafePerformIO)

import Formatting (base, format, left, (%.))

newCuid :: IO Text
newCuid = concatIO [prefix, globalCount] where
    concatIO actions = fmap mconcat (sequence actions)
    prefix = return $ fromString "c"
    globalCount = fmap formatNumber (next counter)

type Counter = IORef Int

counter :: Counter
counter = unsafePerformIO (newIORef 0)

next :: Counter -> IO Int
next c = atomicModifyIORef' c incrementAndWrap where
    incrementAndWrap count = (succ count `mod` countMax, count)
    countMax = formatBase ^ blockSize

formatBase, blockSize :: Int
formatBase = 36
blockSize = 4

formatNumber :: Int -> Text
formatNumber = format (toBlockSize %. toBase) where
    toBlockSize = left blockSize '0'
    toBase = base formatBase
