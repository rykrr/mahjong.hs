module Mahjong.Tile.TileState where

import Mahjong.Tile

--------------------------------------------------------------------------------

data TileState = None           -- Player must discard a tile
               | Drawn Tile     -- Player drew a tile and must discard
               | Discarded Tile -- Player discarded tile, may be picked up
               deriving (Eq, Show)

--------------------------------------------------------------------------------
