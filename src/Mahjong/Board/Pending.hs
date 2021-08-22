module Mahjong.Board.Pending (
    Pending
  , Mahjong.Board.Pending.null
  , empty
  , dismiss
  , findPending
  , hasPriority
) where

import Mahjong.Players as Players

import Mahjong.Tile
import Mahjong.Tile.Melds
import Mahjong.Tile.ActiveTile

import Control.Result
import Helper.List

import Data.Sort

--------------------------------------------------------------------------------

type Priority = Int

newtype Pending = Pending {
    unPending :: [(PlayerId, Priority)]
} deriving (Show)

--------------------------------------------------------------------------------

-- Lower number is higher priority
maxPriority, minPriority :: Priority
maxPriority = 0
minPriority = 9

meldPriority :: MeldType -> Priority
meldPriority meld = case meld of
    Pon   -> 2
    Kan _ -> 2
    Chii  -> 3

--------------------------------------------------------------------------------

findPending :: Players -> ActiveTile -> Pending
findPending players activeTile =
    let actions = Players.map findActions players
     in Pending $ sortOn snd [ toPriority action | action <- actions
                                                 , atLeastOne action ]
  where
    atLeastOne :: (a, [b]) -> Bool
    atLeastOne = not . Prelude.null . snd

    findActions :: Player -> (PlayerId, [MeldType])
    findActions player =
        let melds = checkMelds (activeTile) (position player) (getHand player)
         in (getPlayerId player, melds)

    toPriority :: (PlayerId, [MeldType]) -> (PlayerId, Priority)
    toPriority (id, melds) =
        let priority = foldl (flip $ min . meldPriority) minPriority melds
         in (id, priority)

--------------------------------------------------------------------------------

empty :: Pending
empty = Pending []

null :: Pending -> Bool
null (Pending []) = True
null _            = False

--------------------------------------------------------------------------------

dismiss :: PlayerId -> Pending -> Pending
dismiss id (Pending pending) =
    maybe (Pending pending) (Pending) (removeSingle' ((== id) . fst) pending)

--------------------------------------------------------------------------------

hasPriority :: PlayerId -> Pending -> Bool
hasPriority id (Pending list) = case list of
    []          -> False
    ((id',_):_) -> id' == id

--------------------------------------------------------------------------------
