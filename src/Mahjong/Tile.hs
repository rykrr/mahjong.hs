module Mahjong.Tile where

import Control.Result

--------------------------------------------------------------------------------

data Wind = South
          | East
          | West
          | North
          deriving (Eq, Ord, Show)

data Dragon = Red
            | Blue
            | White
            | Green
            deriving (Eq, Ord, Show)

data Tile = Dragon Dragon
          | Wind Wind
          | Character Int
          | Sticks Int
          | Dots Int
          deriving (Eq, Ord, Show)

type Tiles = [Tile]

--------------------------------------------------------------------------------

winds :: [Wind]
winds = [East, South, West, North]

nextWind :: Wind -> Wind
nextWind wind = case wind of
    East  -> South
    South -> West
    West  -> North
    North -> East

dragons :: [Dragon]
dragons = [Red, White, Blue, Green]

fullTileset :: Tiles
fullTileset = foldl1 (++) $ replicate 4 $
    (foldl1 (++) $ map (flip map [1..9]) [Character, Dots, Sticks])
    ++ (map Wind winds)
    ++ (map Dragon dragons)

--------------------------------------------------------------------------------

isSameType :: Tile -> Tile -> Bool
isSameType (Character _) (Character _) = True
isSameType (Dots      _) (Dots      _) = True
isSameType (Sticks    _) (Sticks    _) = True
isSameType (Dragon    _) (Dragon    _) = True
isSameType (Wind      _) (Wind      _) = True
isSameType _             _             = False

(===) = isSameType
infix 4 ===

--------------------------------------------------------------------------------
