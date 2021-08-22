module Mahjong.Tile.Run where

import Control.Result
import Mahjong.Tile
import Mahjong.Tile.Value
import Data.Sort

--------------------------------------------------------------------------------

newtype Run = Run { unwrapRun :: Tiles }

--------------------------------------------------------------------------------

makeRun :: Tiles -> Result Run
makeRun tiles@(tile@(Suit (SuitTile _ value)):_)
  | all (=== tile) tiles = go (toInt value) (sort tiles)
  | otherwise = Err "Tiles must be of the same suit"
  where
    go _ [] = return (Run tiles)

    go expected ((Suit (SuitTile _ value)):remainder)
      | value' == expected = go (value' + 1) remainder
      | value' <  expected = Err "Duplicate detected"
      | otherwise          = Err "No run found"
      where value' = toInt value

makeRun ((Honor _):_) = Err "Can't make runs from honor tiles"

--------------------------------------------------------------------------------
