-- rykrr note
--   This is an incomplete and inefficient JSON parser implementation;
--   It has not been tested; Please avoid using this at all costs;
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module Data.MiniMap (
    MiniMap
  , MiniVec
  , MiniVal(..)
  , Data.MiniMap.parse
  , Data.MiniMap.lookup
  , Data.MiniMap.lookupConvert
  , Data.MiniMap.lookupMap
  , Data.MiniMap.lookupStr
  , Data.MiniMap.lookupInt
  , Data.MiniMap.member
  , unwrapStr
  , unwrapMap
  , unwrapInt
  , toText
  , toMap
  , toInt
  , isStr
  , isInt
  , isMap
  , isVec
) where

import Prelude hiding (lookup)

import Control.Result
import Control.Monad
import Control.If

import Data.Char (isDigit, isAlpha, isSpace)

import Data.Map as Map
import Data.Text as Text
import Data.Vector as Vec

import Text.Read (readMaybe)

--------------------------------------------------------------------------------

newtype MiniMap = MiniMap { unMiniMap :: Map Text MiniVal }
newtype MiniVec = MiniVec { unMiniVec :: Vector MiniVal }

data MiniVal = Int Int
             | Str Text
             | Map MiniMap
             | Vec MiniVec

--------------------------------------------------------------------------------

puncons :: Text -> Maybe (Char, Text)
puncons text = Text.uncons text >>= \(x,xs) -> return (x, Text.strip xs)

punsnoc :: Text -> Maybe (Text, Char)
punsnoc text = Text.unsnoc text >>= \(xs,x) -> return (Text.strip xs, x)

infix 5 :>
infix 5 :<
pattern x :> xs <- (puncons -> Just (x, xs))
pattern xs :< x <- (punsnoc -> Just (xs, x))
pattern Empty   <- (Text.uncons -> Nothing)

test :: (Char -> Bool) -> Char -> Maybe Char
test fn c
  | fn c      = return c
  | otherwise = Nothing

pattern Alpha c <- (test isAlpha -> Just c)
pattern Digit c <- (test isDigit -> Just c)
pattern Space c <- (test isSpace -> Just c)

--------------------------------------------------------------------------------

instance Show MiniVal where
    show (Map m) = show m
    show (Vec v) = show v
    show (Int i) = show i
    show (Str s) = show s

instance Show MiniMap where
    show minimap
      | Map.null $ unMiniMap minimap = "{}"
      | otherwise = unpack $ brace $
            Prelude.foldl1 (\x y -> x <> ", " <> y)
                         $ Prelude.map showEntry
                                     $ toAscList (unMiniMap minimap)
      where
        quote text = "\"" <> text <> "\""
        brace text = "{" <> text <> "}"

        showEntry (key, value) = (quote key) <> ": " <> (pack $ show value)

instance Show MiniVec where
    show = show . unMiniVec

--------------------------------------------------------------------------------

parse :: Text -> Result MiniVal
parse text = parse' text >>= return . fst

parse' :: Text -> Result (MiniVal, Text)
parse' Empty         = Err "Reached <EOL>"
parse' (Space _:>xs) = parse' xs
parse' input@(x:>xs) = case x of
    '[' -> parseVec xs    >>= \(c,r) -> return (Vec c, r)
    '{' -> parseMap xs    >>= \(c,r) -> return (Map c, r)
    '"' -> parseStr xs    >>= \(c,r) -> return (Str c, r)
    _   -> parseInt input >>= \(c,r) -> return (Int c, r)

parseVec :: Text -> Result (MiniVec, Text)
parseVec Empty     = Err "Reached <EOL> while looking for ']'"
parseVec (']':>xs) = return (MiniVec Vec.empty, xs)

parseVec input = do
    (item, remainder) <- parse' input
    (items, remainder) <- collect remainder [item]
    return (MiniVec (Vec.fromList items), remainder)
  where
    collect (Space _:>xs) items = collect xs items

    collect (']':>xs) items = return (items, xs)
    collect (',':>xs) items = do
        (item, remainder) <- parse' xs
        collect remainder (items Prelude.++ [item])

    collect _ pairs = Err ""

parseMap :: Text -> Result (MiniMap, Text)
parseMap Empty     = Err "Reached <EOL> while looking for '}'"
parseMap ('}':>xs) = return (MiniMap Map.empty, xs)

parseMap input = do
    (pair, remainder) <- parseEntry input
    (pairs, remainder) <- collect remainder [pair]
    return (MiniMap (Map.fromList pairs), remainder)
  where
    collect (Space _:>xs) pairs = collect xs pairs

    collect ('}':>xs) pairs = return (pairs, xs)
    collect (',':>xs) pairs = do
        (pair, remainder) <- parseEntry xs
        collect (Text.strip remainder) (pair:pairs)

    collect _ pairs = Err "Expected ',' or '}' while parsing map"

    parseEntry Empty = Err "Reached <EOL> while parsing map entry"
    parseEntry ('"':>xs) = do
        (key, remainder) <- parseStr xs

        let remainder' = Text.strip remainder

        if' (Text.head remainder' == ':')
            (parse' (Text.tail remainder') >>= \(value, remainder) ->
                return ((key, value), remainder))
            (Err "Expected ':' while parsing map")

    parseEntry _ = Err "Expected '\"' in map key"

parseInt :: Text -> Result (Int, Text)
parseInt Empty = Err "Reached <EOL> while parsing int"
parseInt input = do
    (collected, remainder) <- collect input Text.empty
    maybe (Err $ "Failed to parse int: " <> collected)
          (\value -> Ok (value, remainder))
          (readMaybe (Text.unpack collected))
  where
    collect Empty         collected = return (collected, Text.empty)
    collect input@(x:>xs) collected
      | isDigit x = collect xs (Text.snoc collected x)
      | otherwise = return (collected, input)

parseStr :: Text -> Result (Text, Text)
parseStr input =
    collect input Text.empty
  where
    collect Empty      collected = return (collected, Text.empty)
    collect ('\\':>xs) collected
      | Text.null xs = Err "Reached <EOL> while parsing string"
      | otherwise    = collect (Text.tail xs)
                               (Text.snoc (Text.snoc collected '\\')
                                          (Text.head xs))

    collect ('"':>xs) collected = return (collected, xs)
    collect (x:>xs)   collected = collect xs (Text.snoc collected x)

--------------------------------------------------------------------------------

member :: Text -> MiniMap -> Bool
member key map = Map.member key (unMiniMap map)

lookup :: Text -> MiniMap -> Result MiniVal
lookup key map =
    lookup' (Text.splitOn "." key) (Map map)
  where
    lookup' :: [Text] -> MiniVal -> Result MiniVal
    lookup' []         value     = return value
    lookup' (key:keys) (Map map) = do
        value <- resultFromMaybe ("Key not found")
                               $ Map.lookup key (unMiniMap map)
        lookup' keys value

    lookup' _ _ = Err "Attempted to perform lookup on a non-map object"

lookupConvert :: (MiniVal -> Result a) -> Text -> MiniMap -> Result a
lookupConvert conv key map = Data.MiniMap.lookup key map >>= conv

lookupMap :: Text -> MiniMap -> Result MiniMap
lookupMap = lookupConvert toMap

lookupStr :: Text -> MiniMap -> Result Text
lookupStr = lookupConvert toText

lookupInt :: Text -> MiniMap -> Result Int
lookupInt = lookupConvert toInt

--------------------------------------------------------------------------------

isVec :: MiniVal -> Bool
isVec (Vec _) = True
isVec _       = False

isMap :: MiniVal -> Bool
isMap (Map _) = True
isMap _       = False

isStr :: MiniVal -> Bool
isStr (Str _) = True
isStr _       = False

isInt :: MiniVal -> Bool
isInt (Int _) = True
isInt _       = False

--------------------------------------------------------------------------------

toText :: MiniVal -> Result Text
toText (Str s) = return s
toText _       = Err "MiniVal is not of type Str/Text"

toInt :: MiniVal -> Result Int
toInt (Int i) = return i
toInt _       = Err "MiniVal is not of type Int"

toMap :: MiniVal -> Result MiniMap
toMap (Map m) = return m
toMap _       = Err "MiniVal is not of type Map"

--------------------------------------------------------------------------------

unwrapStr :: MiniVal -> Text
unwrapStr (Str s) = s

unwrapInt :: MiniVal -> Int
unwrapInt (Int x) = x

unwrapMap :: MiniVal -> MiniMap
unwrapMap (Map m) = m

--------------------------------------------------------------------------------
