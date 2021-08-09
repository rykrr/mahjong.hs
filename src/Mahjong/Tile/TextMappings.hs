module Mahjong.Tile.TextMappings (
    toTile
  , toKanType
) where

import Mahjong.Tile.Melds as Tiles
import Prelude hiding (lookup)
import Text.Read (readMaybe)
import Data.Text hiding (zip, map)
import Control.Result

--------------------------------------------------------------------------------

generateStrPairs :: Show a => [a] -> [(Text, a)]
generateStrPairs str = flip zip str $ map (toLower . pack . show) str

dragonStrs = generateStrPairs dragons
windStrs   = generateStrPairs winds

lookup :: [(Text, a)] -> Text -> Result a
lookup [] key = Err ("could not find entry corresponding to: " <> key)
lookup ((k,t):kts) key
  | key == k  = return t
  | otherwise = lookup kts key

--------------------------------------------------------------------------------

toTile :: [Text] -> Result Tile
toTile (a:b:_) = case a of
    "dragon"    -> find dragonStrs Dragon
    "wind"      -> find windStrs   Wind
    "character" -> stoi b Character
    "sticks"    -> stoi b Sticks
    "dots"      -> stoi b Dots
    _           -> Err "no match"
  where
    find str constructor = lookup str b >>= return . constructor
    stoi str constructor = maybe (Err "could not convert string to int")
                                 (\value -> cond value >>= return . constructor)
                                 (readMaybe str :: Maybe Int)

    cond x | inNumericTileRange x = Just x
           | otherwise            = Err "tile value must be 1-9 inclusive"

toTile _ = Err "tile requires 2 arguments"

toKanType :: [Text] -> Result KanType
toKanType (word:_) = case word of
    "open"   -> return Open
    "added"  -> return Added
    "closed" -> return Closed
    _        -> Err "unknown kan type"

toKanType _ = Err "kan type requires 1 argument"

--------------------------------------------------------------------------------
