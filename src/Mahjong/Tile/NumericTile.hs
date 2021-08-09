module Mahjong.Tile.NumericTile where

import Mahjong.Tile
import Control.Result
import Control.If

--------------------------------------------------------------------------------

newtype NumericTile = NumericTile { unwrapNumeric :: Tile } deriving (Show)

--------------------------------------------------------------------------------

errNonNumeric :: Result a
errNonNumeric = Err "tile not numeric (expected sticks, dots, or characters)"

errOutOfRange :: Result a
errOutOfRange = Err "tile value out of range"

--------------------------------------------------------------------------------

numericTiles :: [NumericTile]
numericTiles = map (NumericTile) [(Character 1), (Dots 1), (Sticks 1)]

inNumericTileRange :: Int -> Bool
inNumericTileRange x = 0 < x && x < 10

toNumericTile :: Tile -> Result NumericTile
toNumericTile tile = case tile of
    Dragon _ -> errNonNumeric
    Wind   _ -> errNonNumeric
    _        -> if' (inNumericTileRange (getValue (NumericTile tile)))
                    (return (NumericTile tile))
                    (errOutOfRange)

getValue :: NumericTile -> Int
getValue (NumericTile tile) = case tile of
    Character x -> x
    Sticks    x -> x
    Dots      x -> x

setValue :: NumericTile -> Int -> Result NumericTile
setValue (NumericTile tile) value = toNumericTile $ case tile of
    Character _ -> Character value
    Sticks    _ -> Sticks value
    Dots      _ -> Dots value

--------------------------------------------------------------------------------
