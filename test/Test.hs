module Main where

import Control.Monad (foldM, when)
import Data.Set (empty, insert, size)
import System.Exit (ExitCode(..), exitWith)

import Web.Cuid (Cuid, newCuid, newSlug)

main :: IO ()
main = do
    nCuid <- runCollisionTest newCuid 1200000
    nSlug <- runCollisionTest newSlug 1200000
    case (nCuid, nSlug) of
        (0, 0) -> exitWith ExitSuccess
        _otherwise -> do
            when (nCuid /= 0) $ print ("cuid collisions: " ++ show nCuid)
            when (nSlug /= 0) $ print ("slug collisions: " ++ show nSlug)
            exitWith (ExitFailure 1)

runCollisionTest :: IO Cuid -> Int -> IO Int
runCollisionTest action numberOfCuids = do
    let accumulate set _i = do
            cuid <- action
            return $! insert cuid set
    -- Generate a set containing a bunch of generated CUIDs.
    set <- foldM accumulate empty [0 .. numberOfCuids - 1]
    -- If every element was unique, the set will have the same size as the input.
    return (numberOfCuids - size set)
