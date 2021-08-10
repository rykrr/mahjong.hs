module Server.Room where

import Mahjong

import Data.Map as Map

import Control.Monad
import Control.If

import Control.Concurrent.Chan
import Control.Concurrent.MVar

import System.Random

--------------------------------------------------------------------------------

type RoomID = String

data Room = Room {
    channel :: Chan String
  , players :: [PlayerIdentity]
  , board   :: MVar Board
}

type Rooms = Map RoomID Room

--------------------------------------------------------------------------------

newRoom :: MVar Rooms -> IO (RoomID)
newRoom directory = modifyMVar directory newRoom'
  where
    newRoom' :: Rooms -> IO (Rooms, RoomID)
    newRoom' rooms = do
        code  <- nextCode
        chan  <- newChan
        board <- newEmptyMVar
        let room = Room { channel = chan, players = [], board = board }
        return (insert code room rooms, code)
      where
        nextCode :: IO String
        nextCode = do
            code <- replicateM 4 $ randomRIO ('A', 'Z')
            if' (member code rooms) (nextCode) (return code)

--------------------------------------------------------------------------------

--registerPlayer :: MVar Rooms -> PlayerIdentity

--------------------------------------------------------------------------------

withRoom :: a -> MVar Rooms -> RoomID -> (Room -> IO a) -> IO a
withRoom fallback rooms code fn = withMVar rooms $ \rooms ->
    case Map.lookup code rooms of
        Nothing   -> return fallback
        Just room -> fn room

--------------------------------------------------------------------------------
