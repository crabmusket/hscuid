module Main where

import Data.Set (empty, insert, size)
import Control.Monad (foldM)
import System.Exit (ExitCode(..), exitWith)

import Web.Cuid (newCuid)

main :: IO ()
main = do
    result <- runCollisionTest 1200000
    case result of
        Nothing -> exitWith ExitSuccess
        Just n -> do
            print ("Number of collisions: " ++ show n)
            exitWith (ExitFailure 1)

runCollisionTest :: Int -> IO (Maybe Int)
runCollisionTest numberOfCuids = do
    let accumulate set _i = do
            cuid <- newCuid
            return $! insert cuid set
    -- Generate a set containing a bunch of generated CUIDs.
    set <- foldM accumulate empty [0..numberOfCuids-1]
    -- If every element was unique, the set will have the same size as the input.
    return $ if size set == numberOfCuids
        then Nothing
        else Just (numberOfCuids - size set)
