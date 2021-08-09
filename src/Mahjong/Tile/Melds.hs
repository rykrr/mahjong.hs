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

errExpect tile expected found = Err . Text.pack $
    printf "expected %d %s tiles, found %d" expected (show tile) found

errNoRun base  = Err . Text.pack $
    printf "err no run starting at %s" (show base)

errBadPosition = Err "meld cannot be performed at current position"

errExpectDiscard = Err "requested meld can only be performed on discarded tiles"
errExpectDraw    = Err "requested meld can only be performed on drawn tiles"

--------------------------------------------------------------------------------

consumeRepeats :: Int -> Meld -> Tile -> Hand -> Result Hand
consumeRepeats minimum meld tile (tiles, melds) =
    consume (length $ filter ((==) tile) tiles)
  where
    consume :: Int -> Result Hand
    consume len
      | minimum <= len = Ok (removeInstances minimum tile tiles, meld : melds)
      | otherwise      = errExpect tile minimum len

--------------------------------------------------------------------------------

pon :: MeldFn
pon (Discarded tile) position hand
  | position /= 0 = consumeRepeats 2 (Pon tile) tile hand
  | otherwise     = errBadPosition

pon _ _ _ = errExpectDiscard

--------------------------------------------------------------------------------

kan :: KanType -> MeldFn
kan Added (Drawn tile) 0 (tiles, melds) =
    let meld = (Pon tile)
     in if' (elem meld melds)
            (Ok (tiles, Kan Added tile : removeSingle meld melds))
            (Err "added kan requires an existing pon")

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
    tile' <- toNumericTile tile

    let value  = getValue base'
        values = [ unsafeGetValue t | t <- tile:tiles, t === base ]

    if' (all (\offset -> elem (value + offset) values) [0..2])
        (ret $ removeRun base')
        (errNoRun base)
  where
    ret tiles = return (tile:tiles, (Chii base):melds)

    unsafeGetValue :: Tile -> Int
    unsafeGetValue = getValue . unwrap . toNumericTile

    removeRun :: NumericTile -> Tiles
    removeRun base = Prelude.foldl (del base) tiles [1,2]

    del :: NumericTile -> Tiles -> Int -> Tiles
    del base tiles offset =
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
