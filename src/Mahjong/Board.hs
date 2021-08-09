module Mahjong.Board where

import System.Random.Shuffle

import Mahjong.Board.Event

import Mahjong.Players as Players

import Mahjong.Tile
import Mahjong.Tile.ActiveTile

import Mahjong.Board.Pending as Pending

import Helper.List

import Control.Result
import Control.If

import Data.Text as Text

--------------------------------------------------------------------------------

data Board = Board {
    players        :: Players
  , prevailingWind :: Wind
  , repeatCounter  :: Int
  , roundCounter   :: Int
  , syncCounter    :: Int
  , activeTile     :: Maybe ActiveTile
  , wall           :: Tiles
  , deadWall       :: Tiles
  , lastEvent      :: Event
  , pending        :: Pending
} deriving (Show)

--------------------------------------------------------------------------------

deadWallSize, handSize :: Int
deadWallSize = 14
handSize     = 13

--------------------------------------------------------------------------------

newBoard :: [PlayerIdentity] -> Result Board
newBoard pendingPlayers = do
    players' <- toPlayers pendingPlayers
    return $ Board {
        players        = players'
      , prevailingWind = East
      , repeatCounter  = -1
      , roundCounter   = 0
      , syncCounter    = 0
      , activeTile     = Nothing
      , wall           = []
      , deadWall       = []
      , lastEvent      = NOP
      , pending        = Pending.empty
    }

--------------------------------------------------------------------------------

startRound :: Bool -> Board -> IO (Result Board)
startRound repeat board@(Board { players = ps, prevailingWind = wind }) = do
    tileset <- shuffleM fullTileset

    let (players', tileset')     = setupPlayers tileset
    let (deadWall, (drawn:wall)) = Prelude.splitAt deadWallSize tileset'

    return $ return board {
        prevailingWind = if' repeat wind (nextWind wind)
      , repeatCounter  = if' repeat (repeatCounter board + 1) 0
      , roundCounter   = roundCounter board + 1
      , syncCounter    = syncCounter board + 1
      , players        = players'
      , wall           = wall
      , deadWall       = deadWall
      , activeTile     = Just (Drawn drawn)
      , pending        = findPending players' (Drawn drawn)
      , lastEvent      = PlayerEvent (getPlayerId $ Players.head players')
                                     (Draw $ Just drawn)
    }
  where
    setupPlayers :: Tiles -> (Players, Tiles)
    setupPlayers tiles =
        distributeTiles handSize tiles
                      $ Players.forEach clear (rotateToNext ps)

    rotateToNext :: Players -> Players
    rotateToNext = if' repeat goToDealer rotatePlayerWinds

--------------------------------------------------------------------------------

withPlayers :: Board
            -> (Players -> Result Players)
            -> Result Board

withPlayers board fn = do
    players' <- fn $ players board
    return $ board { players = players' }

--------------------------------------------------------------------------------

withPlayer :: Board
           -> PlayerId
           -> (Player -> Result Player)
           -> Result Board

withPlayer board id fn = do
    player'  <- modifyPlayer (players board) id fn
    players' <- goToPlayer player' id
    return $ board { players = players' }

--------------------------------------------------------------------------------
