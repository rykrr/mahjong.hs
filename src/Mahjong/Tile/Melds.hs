module Mahjong.Tile.Melds (
    Meld(..)
  , Melds
  , MeldFn
  , KanType(..)
  , Hand
  , pon
  , kan
  , chii
  , testForMelds
  , module Mahjong.Tile
  , module Control.Result
) where

import Control.Result
import Control.Monad
import Control.If

import Data.Maybe

import qualified Data.Text as Text

import Text.Printf

import Mahjong.Tile
import Mahjong.Tile.ActiveTile
import Mahjong.Tile.NumericTile

import Helper.List

--------------------------------------------------------------------------------

data KanType = Open
             | Closed
             | Added
             deriving (Eq, Show)

data Meld = Chii Tile
          | Pon Tile
          | Kan KanType Tile
          deriving (Eq, Show)

type Melds = [Meld]
type Hand  = (Tiles, Melds)

type MeldFn = ActiveTile     -- Tile being melded
              -> Int         -- Player's position relative to dealer
              -> Hand        -- Player's hand
              -> Result Hand -- Player's hand if the meld is possible

--------------------------------------------------------------------------------

kanTypes :: [KanType]
kanTypes = [Open, Closed, Added]

--------------------------------------------------------------------------------

errExpect tile expectedCount = Err . Text.pack $
    printf "require %d %s tiles" expectedCount (show tile)

errNoRun base  = Err . Text.pack $
    printf "err no run starting at %s" (show base)

errBadPosition = Err "meld cannot be performed at current position"

errExpectDiscard = Err "requested meld can only be performed on discarded tiles"
errExpectDraw    = Err "requested meld can only be performed on drawn tiles"

--------------------------------------------------------------------------------

consumeRepeats :: Int -> Meld -> Tile -> Hand -> Result Hand
consumeRepeats minimum meld tile (tiles, melds) = do
    tiles <- maybe (errExpect tile minimum) (return)
                   (removeInstances minimum tile tiles)
    return (tiles, meld:melds)

--------------------------------------------------------------------------------

pon :: MeldFn
pon (Discarded tile) position hand
  | position /= 0 = consumeRepeats 2 (Pon tile) tile hand
  | otherwise     = errBadPosition

pon _ _ _ = errExpectDiscard

--------------------------------------------------------------------------------

kan :: KanType -> MeldFn
kan Added (Drawn tile) 0 (tiles, melds) = do
    melds <- resultFromMaybe "Hand must already contain a pon of the same tile"
                           $ removeSingle (Pon tile) melds
    return (tiles, Kan Added tile : melds)

kan Added _ 0 _ = errExpectDraw
kan Added _ _ _ = errBadPosition

kan ktype (Discarded tile) position hand
  | position /= 0 = consumeRepeats 3 (Kan ktype tile) tile hand
  | otherwise     = errBadPosition

kan _ _ _ _ = errExpectDiscard

--------------------------------------------------------------------------------

chii :: Tile -> MeldFn
chii base (Discarded tile) 1 (tiles, melds) = do
    base' <- toNumericTile base
    tiles <- maybe (errNoRun base) (return) (removeRun base')
    return (tiles, (Chii base):melds)
  where
    removeRun :: NumericTile -> Maybe Tiles
    removeRun base = foldM (removeTile base) tiles [0..2]

    removeTile :: NumericTile -> Tiles -> Int -> Maybe Tiles
    removeTile base tiles offset =
        let tile = unwrap $ setValue base (getValue base + offset)
         in removeSingle (unwrapNumeric tile) tiles

chii _ (Discarded _) position _ = errBadPosition
chii _ _             _        _ = errExpectDiscard

--------------------------------------------------------------------------------

testForMelds :: ActiveTile -> Int -> Hand -> Melds
testForMelds _ 0 _ = []
testForMelds tile position hand =
    [ meld | Ok meld <- map testMeld (ponkan ++ chii') ]
  where
    testMeld :: MeldFn -> Result Meld
    testMeld fn = fn tile position hand >>= \(_, (meld:_)) -> Ok meld

    ponkan, chii' :: [MeldFn]
    ponkan = pon : map kan kanTypes
    chii'  = result (\_ -> []) (makeChii) 
                    (toNumericTile $ unwrapActive tile)

    makeChii :: NumericTile -> [MeldFn]
    makeChii tile = let value = getValue tile in
        [ chii (unwrapNumeric t) | offset <- [-1..1]
                                 , Ok t   <- [setValue tile (value + offset)]]

--------------------------------------------------------------------------------
