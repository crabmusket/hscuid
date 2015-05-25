module Main where

import Web.Cuid (newCuid)

main :: IO ()
main = newCuid >>= print
