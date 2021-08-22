{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module Mahjong.Tile.Mappings.Text (parseTile, parseTiles) where

import Mahjong.Tile
import Mahjong.Tile.Value as Value

import Control.Result
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Char (isAlpha, isDigit, digitToInt)

import Helper.List (thereCanBeOnlyOne)

--------------------------------------------------------------------------------

test :: (Char -> Bool) -> Char -> Maybe Char
test fn c
  | fn c      = return c
  | otherwise = Nothing

pattern Alpha c <- (test isAlpha -> Just c)
pattern Digit c <- (test isDigit -> Just c)

infix 5 :>
pattern x :> xs <- (Text.uncons -> Just (x, xs))
pattern Empty   <- (Text.uncons -> Nothing)

--------------------------------------------------------------------------------

suitFromChar :: Char -> Result Suit
suitFromChar c = case c of
    'm' -> return Man
    's' -> return Sou
    'p' -> return Pin
    _   -> Err "Invalid suit designator"

--------------------------------------------------------------------------------

parseTiles :: Text -> Result Tiles
parseTiles text = go [] text []
  where
    go :: [Int] -> Text -> Tiles -> Result Tiles
    go _ "" acc = return acc
    
    go digits ((Digit c):>cs) acc =
        go ((digitToInt c):digits) cs acc
    
    go digits ((Alpha 'z'):>cs) acc = do
        honors <- mapM parseHonor digits
        go [] cs (acc ++ (map Honor honors))
    
    go digits ((Alpha c):>cs) acc = do
        values <- resultFromMaybe "Value out of range"
                                $ mapM Value.fromInt digits
        suit <- suitFromChar c
        go [] cs (acc ++ (map (Suit . SuitTile suit) values))

    parseHonor :: Int -> Result HonorTile
    parseHonor value
      | 1 <= value && value <= 4 = return $ Wind (toEnum (value - 1))
      | 5 <= value && value <= 8 = return $ Dragon (toEnum (value - 5))
      | otherwise = Err "Value out of range (expected [1..8])"

parseTile :: Text -> Result Tile
parseTile text = do
    tiles <- parseTiles text
    resultFromMaybe "Expected exacly one tile" (thereCanBeOnlyOne tiles)

--------------------------------------------------------------------------------
