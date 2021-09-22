{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module Mahjong.Tile.ActiveTile (
    ActiveTile(..)
  , unwrapActive
  , parseActive
) where

import Mahjong.Tile
import Mahjong.Tile.Mappings.Text

import Data.Text as Text
import Control.Result
import Helper.List

--------------------------------------------------------------------------------

data ActiveTile = Drawn Tile
                | Discarded Tile
                deriving (Show, Eq)

--------------------------------------------------------------------------------

unwrapActive :: ActiveTile -> Tile
unwrapActive (Drawn     tile) = tile
unwrapActive (Discarded tile) = tile

--------------------------------------------------------------------------------

mapping :: [(Text, Tile -> ActiveTile)]
mapping = [("Drawn", Drawn), ("Discarded", Discarded)]

parseActive :: Text -> Result ActiveTile
parseActive text = do
    resultFromMaybe "No matching prefix" parse
  where 
    parse :: Maybe ActiveTile
    parse = thereCanBeOnlyOne
        [ mapFn tile | (prefix, mapFn) <- mapping
                     , Just text <- [Text.stripPrefix prefix text]
                     , Ok tile <- [parseTile (Text.strip text)] ]

--------------------------------------------------------------------------------
