{-|
Stability: unstable
Portability: portable

Contains low-level number-formatting functions for CUIDs.
-}
module Web.Cuid.Internal.Formatting (
    blockSize, formatBase,
    formatNumber, formatPadded, formatShort
) where

import Data.Text (Text)
import qualified Data.Text as T

-- | These constants are to do with the desired output formatting of numbers in
-- the CUID.
formatBase, blockSize :: Int
formatBase = 36
blockSize = 4

-- | Expresses the given number as a list of digits in the given base. The
-- "digits" are just integers. The first element of the list is the most
-- significant digit and the last element is the least significant.
--
-- > digitsInBase 10 1234 = [1, 2, 3, 4]
-- > digitsInBase 16 1234 = [4, 13, 2]    -- "0x4D2" in the usual notation
digitsInBase :: Integral a => a -> a -> [a]
digitsInBase base' n' = go base' n' []
    where go base n accum
            | n == 0    = if null accum then [0] else accum
            | n < base  = [n] ++ accum
            | otherwise = go base q ([r] ++ accum)
                where (q, r) = quotRem n base

-- | Converts a single integer to a textual digit. The integer can be any value
-- between 0 and 35, inclusive; the numbers 10 through 35 are converted to the
-- lowercase letters "a" through "z".
numberToDigit :: Integral a => a -> Char
numberToDigit n_
    | n < 0     = error "numberToDigit: input is negative"
    | n < 10    = toEnum (n + 48)
    | n < 36    = toEnum (n + 97 - 10)
    | otherwise = error "numberToDigit: input is too large"
    where n = fromIntegral n_

-- | Returns the textual representation of the given integer in the given base.
--
-- > formatInBase 10 999 = "999"
-- > formatInBase 16 999 = "3e7"
-- > formatInBase 36 999 = "rr"
formatInBase :: Integral a => a -> a -> Text
formatInBase base n = T.pack $ map numberToDigit $ digitsInBase base n

-- | Returns the textual representation of the given integer in the given base,
-- padded on the left with zeroes so that the string is /at least/ the given
-- number of digits long.
--
-- > formatPaddedInBase 10 3 1 = "001"
-- > formatPaddedInBase 10 3 12 = "012"
-- > formatPaddedInBase 10 3 123 = "123"
-- > formatPaddedInBase 10 3 1234 = "1234"
formatPaddedInBase :: Integral a => a -> a -> a -> Text
formatPaddedInBase base d n = T.justifyRight digits '0' $ formatInBase base n
    where digits = fromIntegral d

-- | Returns the textual representation of the first (i.e. most significant) two
-- digits of the given integer in the given base. If the number is only one
-- digit long then the resulting Text will be only one character long.
--
-- > formatShortInBase 10 8 = "8"
-- > formatShortInBase 10 86 = "86"
-- > formatShortInBase 10 867 = "86"
formatShortInBase :: Integral a => a -> a -> Text
formatShortInBase base n = T.take 2 $ formatInBase base n

-- | 'formatInBase' specialized to use the application's value of 'formatBase'.
formatNumber :: Int -> Text
formatNumber = formatInBase formatBase

-- | 'formatPaddedInBase' specialized to use the application's values of
-- 'formatBase' and 'blockSize'.
formatPadded :: Int -> Text
formatPadded = formatPaddedInBase formatBase blockSize

-- | 'formatShortInBase' specialized to use the application's value of
-- 'formatBase'.
formatShort :: Int -> Text
formatShort = formatShortInBase formatBase
