module Main where

import System.Exit (exitSuccess, exitFailure)
import System.IO

import Mahjong.Tile
import Mahjong.Tile.Melds (Melds, Hand, checkMelds)
import Mahjong.Tile.Mappings.Text
import Mahjong.Tile.ActiveTile

import Control.Result
import Control.Monad as Monad

import Data.MiniMap
import qualified Data.Vector as Vector
import Data.Text (Text, pack, unpack)

perror :: Text -> IO ()
perror text = hPutStrLn stderr ("\x1b[0;31m" <> unpack text <> "\x1b[0m" )

main :: IO ()
main = do
    input <- readFile "test/tile/cases.json"

    -- TODO: Do a less sketchy job here
    let tests = unMiniVec $ unwrapVec $ unwrap $ parse $ pack input
    --let result = foldM (\prev test -> prev >> runTest (unwrapMap test)) (Ok ()) tests
    let result = mapM (runTest . unwrapMap) tests

    case result of
        Ok _  -> exitSuccess
        Err e -> do
            perror "Test Suite Failed:"
            perror e
            exitFailure

runTest :: MiniMap -> Result ()
runTest m = do
    name <- lookupStr "name" m
    tiles <- lookupStr "hand.tiles" m >>= parseTiles
    -- TODO: Add melds when detecting win conditions
    -- melds <- lookupVec "hand.melds" m
    let hand = (tiles, [])

    cases <- lookupVec "cases" m >>= \x -> Vector.mapM toMap (unMiniVec x)
    Vector.mapM_ (runTestCase name hand) cases

  where
    runTestCase :: Text -> Hand -> MiniMap -> Result ()
    runTestCase name (tiles, melds) m = do
        activeTile <- lookupStr "active" m >>= parseActive
        Err ("Case " <> name <> ": " <> (pack $ show $ activeTile))

