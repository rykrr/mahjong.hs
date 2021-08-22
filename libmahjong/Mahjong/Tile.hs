{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module Mahjong.Tile where

import Control.Monad
import Control.Result

import Mahjong.Tile.Value as Value

--------------------------------------------------------------------------------

data Wind = South
          | East
          | West
          | North
          deriving (Show, Eq, Ord, Enum, Bounded)

data Dragon = Red
            | Blue
            | White
            | Green
            deriving (Eq, Ord, Enum, Show)

data HonorTile = Wind Wind | Dragon Dragon
               deriving (Eq, Ord, Show)

data Suit = Man | Sou | Pin
          deriving (Eq, Ord, Enum)

data SuitTile = SuitTile Suit Value
              deriving (Eq, Ord)

data Tile = Honor HonorTile
          | Suit  SuitTile
          deriving (Eq, Ord)

type Tiles = [Tile]

--------------------------------------------------------------------------------

winds :: [Wind]
winds = [East, South, West, North]

nextWind :: Wind -> Wind
nextWind North = East
nextWind wind  = succ wind

dragons :: [Dragon]
dragons = [Red, White, Blue, Green]

fullTileset :: Tiles
fullTileset = map (Honor . Wind) winds

--------------------------------------------------------------------------------

isSameType :: Tile -> Tile -> Bool
isSameType (Suit  (SuitTile x _)) (Suit  (SuitTile y _)) = x == y
isSameType (Honor (Wind _))       (Honor (Wind _))       = True
isSameType (Honor (Dragon _))     (Honor (Dragon _))     = True
isSameType _                      _                      = False

(===) = isSameType
infix 4 ===

--------------------------------------------------------------------------------

approxEq :: Tile -> Tile -> Bool
approxEq (Honor x)             (Honor y)             = x == y
approxEq (Suit (SuitTile a x)) (Suit (SuitTile b y)) = a == b
                                                       && toInt x == toInt y
approxEq _ _ = False

(=~=) = approxEq
infix 4 =~=

--------------------------------------------------------------------------------

suitTile :: Suit -> Value -> Tile 
suitTile suit value = (Suit (SuitTile suit value))

filterSuitTiles :: Tiles -> [SuitTile]
filterSuitTiles tiles = [ tile | Suit tile <- tiles ]

getValue :: SuitTile -> Value
getValue (SuitTile _ value) = value

containsRed :: Tiles -> Bool
containsRed tiles =
    any (\tile -> getValue tile == RedFive)
        (filterSuitTiles tiles)

--------------------------------------------------------------------------------

instance Show Tile where
    show (Suit (SuitTile suit value)) = show value <>
        case suit of
            Man -> "m"
            Sou -> "s"
            Pin -> "p"

    show (Honor (Dragon dragon)) = show (fromEnum dragon + 5) <> "z"
    show (Honor (Wind wind))     = show (fromEnum wind   + 1) <> "z"

--------------------------------------------------------------------------------
