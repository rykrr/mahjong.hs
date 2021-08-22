module Mahjong.Tile.Value where

import Control.Result

import Data.Map as Map (fromList, lookup, (!))
import Data.Text (Text, pack)

--------------------------------------------------------------------------------

data Value = One     | Two | Three | Four  | Five 
           | RedFive | Six | Seven | Eight | Nine
           deriving (Eq, Ord)

--------------------------------------------------------------------------------

instance Show Value where
    show RedFive = "0"
    show value   = show (toInt value)

--------------------------------------------------------------------------------

values, fives, nonFives :: [Value]
values   = [One, Two, Three, Four, Five, RedFive, Six, Seven, Eight, Nine]
fives    = [Five, RedFive]
nonFives = filter (flip notElem fives) values

--------------------------------------------------------------------------------

toInt :: Value -> Int
toInt = (!) (fromList $ zip values ([1..5] ++ [5..9]))

fromInt :: Int -> Maybe Value
fromInt = flip Prelude.lookup (zip ([1..5] ++ [0] ++ [6..9]) values)

inRange :: Int -> Bool
inRange x = 0 < x && x < 10

--------------------------------------------------------------------------------

fromChar :: Char -> Maybe Value
fromChar = flip Map.lookup 
              $ fromList [ (head (show value), value) | value <- values ]

--------------------------------------------------------------------------------
