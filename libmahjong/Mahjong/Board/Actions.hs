module Mahjong.Board.Actions (doAction) where

import Mahjong.Board
import Mahjong.Board.Event as Event
import Mahjong.Board.Pending as Pending

import Mahjong.Tile.Melds as Melds
import Mahjong.Tile.Mappings.Text
import Mahjong.Tile.ActiveTile

import Mahjong.Players as Players
import Mahjong.Players.Player as Player

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
            tileStr <- Mini.lookupStr "discard" args
            tile <- parseTile tileStr

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
        meldStr <- Mini.lookupStr "type" meldArgs
        tileStr <- Mini.lookupStr "tiles" meldArgs
        tiles <- parseTiles tileStr

        meldFn <- getMeldFn (Text.words meldStr)
        board <- withPlayer board id (modifyHand (meldFn activeTile tiles))

        let hand = Player.getHand $ Players.head $ players board
        let meld = Prelude.head (snd hand)
        return $ board { lastEvent = PlayerEvent id (Event.Meld meld) }

    getMeldFn :: [Text] -> Result MeldFn
    getMeldFn ("kan":kanTypeStr:[]) = case Text.toLower kanTypeStr of
        "open"   -> return (kan Open)
        "closed" -> return (kan Closed)
        _        -> Err "invalid kan type"

    getMeldFn ("chii":[]) = return chii
    getMeldFn ("pon":[])  = return pon

    getMeldFn _ = Err "Unknown meld (or bad meld specifier)"


doAction board@(Board { activeTile = Nothing }) id args = do
    action <- getActionText args
    case action of
        "discard" -> do
            tileStr <- Mini.lookupStr "discard" args
            tile <- parseTile tileStr

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
