module Mahjong.Tile.MiniMappings (toTile) where

import Mahjong.Tile.Melds as Tiles
import Mahjong.Tile.NumericTile
import Prelude hiding (lookup)
import Text.Read (readMaybe)
import Data.MiniMap as Mini
import Data.Text as Text hiding (map, zip)
import Control.Result

--------------------------------------------------------------------------------

generateStrPairs :: Show a => [a] -> [(Text, a)]
generateStrPairs str = flip zip str $ map (toLower . pack . show) str

dragonStrs = generateStrPairs dragons
windStrs   = generateStrPairs winds

strLookup :: Text -> [(Text, a)] -> Result a
strLookup key [] = Err ("could not find entry corresponding to: " <> key)
strLookup key ((k,t):kts)
  | key == k  = return t
  | otherwise = strLookup key kts

--------------------------------------------------------------------------------

toTile :: MiniVal -> Result Tile
toTile (Map m) = do
    tileType  <- Mini.lookupStr "type" m
    tileValue <- Mini.lookup "value" m

    case tileType of
      "dragon"    -> fromText tileValue dragonStrs Dragon
      "wind"      -> fromText tileValue windStrs   Wind
      "character" -> fromInt  tileValue Character
      "sticks"    -> fromInt  tileValue Sticks
      "dots"      -> fromInt  tileValue Dots
      _           -> Err "unknown tile type given"
  where
    fromText (Str key) map constructor =
        strLookup key map >>= return . constructor

    fromText _ _ _ = Err "tile type must be text for non-numeric tiles"

    fromInt (Int value) constructor
      | inNumericTileRange value = return $ constructor value

    fromInt _ _ = Err "tile type must be int for numeric tiles"

toTile _ = Err "toTile expected a map"

--------------------------------------------------------------------------------
