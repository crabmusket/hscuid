{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (foldM, when)
import Data.Set (empty, insert, size)
import Data.Text (Text, append)
import qualified Data.Text.IO
import System.Exit (ExitCode(..), exitWith)

import Web.Cuid (Cuid, newCuid, newSlug)

main :: IO ()
main = do
    putStrLn ""
    putTxtLn . (append "example cuid: ") =<< newCuid
    putTxtLn . (append "example slug: ") =<< newSlug
    putStrLn "Running collision test..."
    cuidCollisions <- runCollisionTest newCuid 1200000
    slugCollisions <- runCollisionTest newSlug 1200000
    case (cuidCollisions, slugCollisions) of
        (0, 0) -> do
            putStrLn "Collision test passed"
            exitWith ExitSuccess
        _otherwise -> do
            putStrLn "Collision test failed"
            when (cuidCollisions /= 0) $ print ("cuid collisions: " ++ show cuidCollisions)
            when (slugCollisions /= 0) $ print ("slug collisions: " ++ show slugCollisions)
            exitWith (ExitFailure 1)

-- We test the CUID generator by generating a lot of ids and putting them into
-- a huge set. Since sets retain only unique elements, the size of the set will
-- equal the number of CUIDs generated iff they were all unique.
runCollisionTest :: IO Cuid -> Int -> IO Int
runCollisionTest generator inputSize = do
    set <- foldM build empty [0 .. inputSize - 1]
    return (inputSize - size set)
    where
        build set _i = do
            cid <- generator
            return $! insert cid set

putTxtLn :: Text -> IO ()
putTxtLn = Data.Text.IO.putStrLn
