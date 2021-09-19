module Mahjong.Players.Player where

import Data.Sort
import Data.Text
import System.Random
import Control.Monad
import Control.Result
import Mahjong.Tile.Melds
import Helper.List

--------------------------------------------------------------------------------

type PlayerId   = String
type AuthSecret = String

data PlayerIdentity = PlayerIdentity {
    playerId :: PlayerId
  , secret   :: AuthSecret
  , name     :: Text
} deriving (Show, Eq)

data Player = Player {
    playerIdentity :: PlayerIdentity
  , score          :: Int
  , position       :: Int
  , seatWind       :: Wind
  , revealedTiles  :: Melds
  , concealedTiles :: Tiles
  , declaredRiichi :: Bool
} deriving (Show, Eq)

--------------------------------------------------------------------------------

newPlayer :: Text -> IO PlayerIdentity
newPlayer name = do
    id     <- replicateM 5 $ randomRIO ('A', 'Z')
    secret <- replicateM 5 $ randomRIO ('A', 'Z')
    return $ PlayerIdentity {
        playerId = id
      , secret   = secret
      , name     = name
    }

toPlayer :: PlayerIdentity -> Player
toPlayer identity =
    Player {
        playerIdentity = identity
      , score          = 0
      , position       = 0
      , seatWind       = East
      , revealedTiles  = []
      , concealedTiles = []
      , declaredRiichi = False
    }

--------------------------------------------------------------------------------

getPlayerId :: Player -> PlayerId
getPlayerId = playerId . playerIdentity

getHand :: Player -> Hand
getHand player@(Player { concealedTiles = c, revealedTiles = r }) = (c,r)

modifyHand :: (Int -> Hand -> Result Hand) -> Player -> Result Player
modifyHand fn player = do
    hand <- fn (position player) (getHand player)
    return $ player { concealedTiles = fst hand, revealedTiles = snd hand }

--------------------------------------------------------------------------------

clear :: Player -> Player
clear player =
    player { concealedTiles = [], revealedTiles = [], declaredRiichi = False }

addTile :: Tile -> Player -> Player
addTile tile player@(Player{ concealedTiles = concealed }) =
    player { concealedTiles = insert tile concealed }

addTiles :: Tiles -> Player -> Player
addTiles tiles player@(Player{ concealedTiles = concealed }) =
    player { concealedTiles = sort (tiles ++ concealed) }

removeTile :: Tile -> Player -> Result Player
removeTile tile player@(Player{ concealedTiles = c }) = do
    tiles <- resultFromMaybe "Tile is not in player's hand"
                           $ removeSingle tile c
    return player { concealedTiles = tiles }

replaceTile :: Tile -> Tile -> Player -> Result Player
replaceTile target replacement player
  | target == replacement = return player
  | otherwise = removeTile target player >>=
                    return . addTile replacement

--------------------------------------------------------------------------------
