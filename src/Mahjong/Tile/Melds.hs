{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module Mahjong.Tile.Melds (
    Meld(..)
  , Melds
  , MeldFn
  , MeldType(..)
  , KanType(..)
  , Hand
  , pon
  , kan
  , chii
  , checkMelds
  , module Mahjong.Tile
  , module Control.Result
) where

import Control.Result
import Control.Monad
import Control.If

import Data.Maybe
import Data.Sort

import qualified Data.Text as Text

import Text.Printf

import Mahjong.Tile
import Mahjong.Tile.Run
import Mahjong.Tile.Value
import Mahjong.Tile.ActiveTile
import Mahjong.Tile.TestResult

import Helper.List

--------------------------------------------------------------------------------

data KanType = Open | Closed | Added
             deriving (Eq, Show)

data MeldType = Chii | Pon | Kan KanType
              deriving (Eq, Show)

type ContainsRed = Bool

data Meld = Meld MeldType Tile ContainsRed
          deriving (Eq, Show)

type Melds = [Meld]
type Hand  = (Tiles, Melds)

type MeldFn = ActiveTile     -- Tile in play
              -> Tiles       -- Tiles to meld
              -> Int         -- Player's position relative to dealer
              -> Hand        -- Player's hand
              -> Result Hand -- Player's hand if the meld is possible

type TestFn = ActiveTile     -- Tile in play
              -> Int         -- Player's position relative to dealer
              -> Hand        -- Player's hand
              -> TestResult  -- Whether the move was accepted or rejected

--------------------------------------------------------------------------------

kanTypes :: [KanType]
kanTypes = [Open, Closed, Added]

meldTypes :: [MeldType]
meldTypes = [Chii, Pon] ++ map Kan kanTypes

--------------------------------------------------------------------------------

pattern NotCurrentPlayer <- ((== 0) -> False)
pattern CurrentPlayer <- 0
pattern NextPlayer <- 1

--------------------------------------------------------------------------------

errNoRun base =
    Text.pack $ printf "err no run starting at %s\n" (show base)

errCount minimum tile =
    Text.pack $ printf "At least %d %s required \n" minimum (show tile)

errBadPosition   = "Action cannot be performed at current position"
errExpectDiscard = "Action can only be performed on discarded tiles"
errExpectDraw    = "Action can only be performed on drawn tiles"
errBadTiles      = "Action could not be performed with specified tiles"

errCheck :: Text.Text -> Text.Text
errCheck = (<>) "Precondition check did not detect invalid request in "

--------------------------------------------------------------------------------

hasRepeats :: Int -> Tile -> Tiles -> TestResult
hasRepeats minimum tile tiles =
    fromBool (errCount minimum tile)
             (minimum <= length (filter (=~= tile) tiles))

consumeRepeats :: Int -> Tile -> Tiles -> Hand -> Result Tiles
consumeRepeats len tile tilesToRemove (tiles, _)
  | len /= length tilesToRemove  = Err "Invalid number of tiles to remove given"
  | all (=~= tile) tilesToRemove = resultFromMaybe (errBadTiles)
                                                   (remove tilesToRemove tiles)
  | otherwise = Err errBadTiles

--------------------------------------------------------------------------------

checkPon :: TestFn
checkPon (Discarded tile) NotCurrentPlayer (tiles, _) = hasRepeats 2 tile tiles

checkPon (Discarded _) CurrentPlayer _ = Reject errBadPosition
checkPon _             _             _ = Reject errExpectDiscard

pon :: MeldFn
pon activeTile@(Discarded tile) tilesToRemove position hand@(_, melds) = do
    tiles <- asResult (consumeRepeats 2 tile tilesToRemove hand)
                      (checkPon activeTile position hand)
    let isRed = containsRed (tile:tilesToRemove)
    return (tiles, (Meld Pon tile isRed):melds)

pon _ _ _ _ = Err errExpectDiscard

--------------------------------------------------------------------------------

checkKan :: KanType -> TestFn
checkKan Open (Discarded tile) NextPlayer (tiles, _) = hasRepeats 3 tile tiles
checkKan Open (Discarded _)    _          _          = Reject errBadPosition
checkKan Open _                _          _          = Reject errExpectDiscard

checkKan Closed (Drawn tile) CurrentPlayer (tiles, _) = hasRepeats 3 tile tiles
checkKan Closed (Drawn _)    _             _          = Reject errBadPosition
--checkKan Closed _            _             _          = Reject errExpectDraw

checkKan Added (Drawn tile) CurrentPlayer (_, melds) =
    fromBool ("Added Kan requires a corresponding Pon")
             (any (\red -> elem (Meld Pon tile red) melds) [False, True])

checkKan Added (Drawn _) _ _ = Reject errBadPosition
--checkKan Added _         _ _ = Reject errExpectDraw

checkKan _ _ _ _ = Reject errExpectDraw

kan :: KanType -> MeldFn
kan kanType activeTile tilesToRemove position hand@(tiles, melds) =
    asResult (kan' kanType activeTile)
             (checkKan kanType activeTile position hand)
  where
    kan' :: KanType -> ActiveTile -> Result Hand
    kan' Added (Drawn tile) = do
        let isRed = containsRed tilesToRemove
        let meldToRemove = Meld Pon tile isRed
        melds <- resultFromMaybe (errBadTiles)
                                 (removeSingle meldToRemove melds)
        return (tiles, (Meld (Kan Added) tile isRed):melds)

    kan' kanType _ = do
        let tile = (unwrapActive activeTile)
        let isRed = containsRed (tile:tilesToRemove)
        tiles <- consumeRepeats 3 tile tilesToRemove hand
        return (tiles, (Meld (Kan kanType) tile isRed):melds)

    kan' _ _ = Err (errCheck "kan'")

--------------------------------------------------------------------------------

checkChii :: TestFn
checkChii (Discarded tile@(Suit (SuitTile suit value))) NextPlayer (tiles, _) =
    scan [ elem (toInt value + offset) values | offset <- [-2..2] ]
  where
    scan :: [Bool] -> TestResult
    scan (True:True:True:_) = Accept
    scan (_:xs)             = scan xs
    scan _                  = Reject "No valid runs found"

    values :: [Int]
    values = sort [ toInt v | Suit (SuitTile s v) <- (tile:tiles), suit == s ]

checkChii (Discarded _) _ _ = Reject errBadPosition
checkChii _             _ _ = Reject errExpectDiscard

chii :: MeldFn
chii activeTile tilesToRemove position hand@(tiles, melds)
  | length tilesToRemove == 3 =
        asResult (chii' activeTile) (checkChii activeTile position hand)
  | otherwise = Err "Too many tiles specified for chii"
  where
    chii' :: ActiveTile -> Result Hand
    chii' (Discarded tile) = do
        run <- makeRun tilesToRemove >>= return . unwrapRun
        tiles <- resultFromMaybe (errBadTiles)
                                 (remove tilesToRemove (tile:tiles))
        let isRed = containsRed tilesToRemove
        return (tiles, (Meld Chii (head run) isRed):melds)

    chii' (Drawn _) = Err (errCheck "chii'")

--------------------------------------------------------------------------------

meldChecks :: [(TestFn, MeldType)]
meldChecks = [(checkChii, Chii), (checkPon, Pon)]
             ++ map (\ktype -> (checkKan ktype, Kan ktype)) kanTypes

checkMelds :: ActiveTile -> Int -> Hand -> [MeldType]
checkMelds _ CurrentPlayer _ = []
checkMelds activeTile position hand =
    [ meldType | (check, meldType) <- meldChecks
               , check activeTile position hand == Accept ]

--------------------------------------------------------------------------------
