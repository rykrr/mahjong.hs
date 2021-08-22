module Tile where

import System.Exit (exitSuccess, exitFailure)

import Control.Result
import Data.MiniMap

main :: IO ()
main = do
    input <- readFile "Tile.hs"
    printStrLn input
    exitSuccess
