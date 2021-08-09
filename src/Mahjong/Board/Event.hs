module Mahjong.Board.Event where

import Mahjong.Tile.Melds as Tiles
import Mahjong.Players.Player as Player

--------------------------------------------------------------------------------

data PlayerEvent = Meld       Tiles.Meld
                 | Draw      (Maybe Tiles.Tile)
                 | Discard    Tiles.Tile
                 deriving (Show)

data Event = NOP
           | PlayerEvent Player.PlayerId PlayerEvent
           deriving (Show)

--------------------------------------------------------------------------------
