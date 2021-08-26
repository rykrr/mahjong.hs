module Main where

import System.Exit (exitSuccess, exitFailure)

import Control.Result
import Data.MiniMap
import Data.Text (pack)

main :: IO ()
main = do
    input <- readFile "test/tile/cases.json"
    putStrLn $ show $ parse $ pack input
    exitSuccess
