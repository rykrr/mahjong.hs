module Mahjong.Tile.TestResult where

import Control.Result
import Data.Text

-------------------------------------------------------------------------------

data TestResult = Accept | Reject Text
                deriving (Show, Eq)

-------------------------------------------------------------------------------

toBool :: TestResult -> Bool
toBool  Accept    = True
toBool (Reject _) = False

fromBool :: Text -> Bool -> TestResult
fromBool _ True  = Accept
fromBool e False = Reject e

------------------------------------------------------------------------------

asResult :: Result a -> TestResult -> Result a
asResult x  Accept    = x
asResult _ (Reject e) = Err e

-------------------------------------------------------------------------------
