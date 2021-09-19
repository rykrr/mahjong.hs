module Main where

import System.Exit (exitSuccess, exitFailure)

import Mahjong.Tile
import Mahjong.Tile.Melds (Melds, Hand, checkMelds)
import Mahjong.Tile.Mappings.Text
import Mahjong.Tile.ActiveTile

import Control.Result
import Control.Monad as Monad

import Data.MiniMap
import Data.Vector as Vector
import Data.Text (pack)

main :: IO ()
main = do
    input <- readFile "test/tile/cases.json"

    -- TODO: Do a less sketchy job here
    let tests = unMiniVec $ unwrapVec $ unwrap $ parse $ pack input
    Monad.mapM (putStrLn . show . test . unwrapMap) tests

    exitSuccess

test :: MiniMap -> Result ()
test m = do
    name <- lookupStr "name" m
    tiles <- lookupStr "hand.tiles" m >>= parseTiles
    -- TODO: Add melds when detecting win conditions
    -- melds <- lookupVec "hand.melds" m
    let hand = (tiles, [])

    cases <- lookupVec "cases" m >>= \x -> Vector.mapM toMap (unMiniVec x)
    Vector.mapM_ (runTestCase hand) cases

  where
    runTestCase :: Hand -> MiniMap -> Result ()
    runTestCase (tiles, melds) m = do
        activeTile <- lookupStr "active" m >>= parseActive
        checkMelds 
        Err (pack $ show $ activeTile)

