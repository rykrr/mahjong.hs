module Mahjong.Tile.ActiveTile where

import Mahjong.Tile

--------------------------------------------------------------------------------

data ActiveTile = Drawn Tile
                | Discarded Tile
                deriving (Show)

--------------------------------------------------------------------------------

unwrapActive :: ActiveTile -> Tile
unwrapActive (Drawn     tile) = tile
unwrapActive (Discarded tile) = tile

--------------------------------------------------------------------------------
