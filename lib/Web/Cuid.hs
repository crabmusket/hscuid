{-# LANGUAGE CPP #-}

{-|
Stability: stable
Portability: portable

You can generate a new CUID or slug inside any IO-enabled monad using 'newCuid'
and 'newSlug'.
-}
module Web.Cuid (
    Cuid, newCuid,
    Slug, newSlug
) where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String (fromString)
import Data.Text (Text)

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid, mconcat)
#endif

import Web.Cuid.Internal

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
    time = liftM formatNumber getTimestamp

    -- To avoid collisions in the same process, add a global counter to each ID.
    count = liftM formatPadded getNextCount

    -- To avoid collisions between separate machines, generate a 'fingerprint'
    -- from details which are hopefully unique to this machine - PID and hostname.
    fingerprint = return myFingerprint

    -- And some actual randomness for good measure.
    random = liftM formatPadded getRandomValue

-- | A Slug is not a Cuid. But it is also a strict Text.
type Slug = Text

-- | A slug is a shorter piece of text generated using some of the same
-- techniques as CUIDs.
newSlug :: MonadIO m => m Slug
newSlug = concatResults [time, count, fingerprint, random] where
    time = liftM formatShort getTimestamp
    count = liftM formatPadded getNextCount
    random = liftM formatShort getRandomValue
    fingerprint = return myFingerprint

-- Evaluate IO actions and concatenate their results.
concatResults :: (MonadIO m, Monoid a) => [IO a] -> m a
concatResults actions = liftM mconcat (liftIO $ sequence actions)
