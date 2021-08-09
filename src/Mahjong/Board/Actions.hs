module Mahjong.Board.Actions (doAction) where

import Mahjong.Board
import Mahjong.Board.Event
import Mahjong.Board.Pending as Pending

import Mahjong.Tile.Melds as Melds
import Mahjong.Tile.MiniMappings
import Mahjong.Tile.ActiveTile

import Mahjong.Players as Players

import Data.MiniMap as Mini
import Data.Text as Text
import Data.Map

--------------------------------------------------------------------------------

doAction :: Board -> PlayerId -> MiniMap -> Result Board
doAction board@(Board { activeTile = Just _, pending = pending }) id _
  | not $ (Pending.null pending) || (hasPriority id pending) =
        Err "Player does not have priority"

doAction board@(Board { players = players, pending = pending }) id _
  | (Pending.null pending) && (id /= getPlayerId (Players.head players)) =
        Err "Not the current player"

doAction board@(Board { activeTile = Just activeTile }) id args = do
    action <- getActionText args
    case action of
        "dismiss" -> return board { pending = dismiss id (pending board) }
        "discard" -> do
            tileMap <- Mini.lookup "discard" args
            tile <- toTile tileMap

            let board' = board {
                activeTile = Just (Discarded tile)
              , lastEvent  = PlayerEvent id (Discard tile)
            }

            withPlayer board' id (replaceTile tile (unwrapActive activeTile))

        "meld" -> do
            meldArgs <- Mini.lookupMap "meld" args
            handleMeld meldArgs

        _ -> Err "unknown action"
  where
    handleMeld :: MiniMap -> Result Board
    handleMeld meldArgs = do
        meldType <- Mini.lookupStr "type" meldArgs

        (meldFn, meldInfo) <- getMeld meldArgs meldType

        let meld   = Meld (meldInfo (unwrapActive activeTile))
        let board' = board { lastEvent = PlayerEvent id meld }

        withPlayer board' id (modifyHand (meldFn activeTile))

    getMeld :: MiniMap -> Text -> Result (MeldFn, Tile -> Meld)
    getMeld meldArgs meldType = case meldType of
        "chii" -> do
            tileMap <- Mini.lookup "chii" meldArgs
            base <- toTile tileMap
            return (chii base, Chii)

        "kan" -> do
            kanType <- Mini.lookupStr "kan" meldArgs
            kanType <- case Text.toLower kanType of
                "open"   -> return Open
                "closed" -> return Closed
                _        -> Err "invalid kan type"
            return (kan kanType, Kan kanType)

        "pon" -> return (pon, Pon)
        _     -> Err "unknown meld"

doAction board@(Board { activeTile = Nothing }) id args = do
    action <- getActionText args
    case action of
        "discard" -> do
            tileMap <- Mini.lookup "discard" args
            tile <- toTile tileMap

            let board' = board {
                activeTile = Just (Discarded tile)
              , lastEvent  = PlayerEvent id (Discard tile)
            }

            withPlayer board' id (removeTile tile)

        _ -> Err "Unknown action"

--------------------------------------------------------------------------------

getActionText :: MiniMap -> Result Text
getActionText args = Mini.lookupStr "action" args

--------------------------------------------------------------------------------
