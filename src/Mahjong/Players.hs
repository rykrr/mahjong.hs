module Mahjong.Players (
    Players
  , numPlayers
  , toPlayers
  , findPlayer
  , rotatePlayers
  , rotatePlayerWinds
  , modifyPlayer
  , goToPlayer
  , goToDealer
  , distributeTiles
  , Mahjong.Players.filter
  , Mahjong.Players.forEach
  , Mahjong.Players.map
  , Mahjong.Players.head
  , Mahjong.Players.toList
  , module Mahjong.Players.Player
  , module Control.Result
) where

import Mahjong.Players.Player
import Mahjong.Tile

import Control.Result

import Data.Vector as Vec hiding (length)
import Data.Maybe

--------------------------------------------------------------------------------

newtype Players = Players { getVector :: Vector Player } deriving (Show)

numPlayers :: Int
numPlayers = 4

--------------------------------------------------------------------------------

toPlayers :: [PlayerIdentity] -> Result Players
toPlayers identities
  | length identities == numPlayers =
      let players = Prelude.map toPlayer identities
       in return $ rotatePlayerWinds (Players (fromList players))

  | otherwise = Err "list must contain 4 players"

--------------------------------------------------------------------------------

findPlayer :: Players -> PlayerId -> Result (Int, Player)
findPlayer players id =
    let players' = getVector players
     in maybe (Err "player not found")
              (\index -> Ok (index, players' ! index))
              (findIndex ((== id) . getPlayerId) players')

--------------------------------------------------------------------------------

modifyPlayer :: Players
             -> PlayerId
             -> (Player -> Result Player)
             -> Result Players

modifyPlayer players id fn = do
    (index, player) <- findPlayer players id
    fn player >>= \player' ->
        return $ Players $ (getVector players) // [(index, player')]

--------------------------------------------------------------------------------

rotatePlayers :: Players -> Int -> Players
rotatePlayers players n =
    updatePlayers $ rotate (getVector players) (mod n 4)
  where
    rotate :: Vector Player -> Int -> Vector Player
    rotate players n
      | n == 0    = players
      | otherwise = rotate (Vec.snoc (Vec.tail players)
                                     (Vec.head players))
                           (n - 1)

    updatePlayers :: Vector Player -> Players
    updatePlayers players = Players $ Vec.imap updatePlayer players

    updatePlayer :: Int -> Player -> Player
    updatePlayer index player = player { position = index }

--------------------------------------------------------------------------------

rotatePlayerWinds :: Players -> Players
rotatePlayerWinds players =
    Players $ Vec.map (\(wind, player) -> player { seatWind = wind })
                      (Vec.zip (Vec.fromList winds)
                               (getVector players'))
  where
    players' :: Players
    players' = rotatePlayers (goToDealer players) 1

--------------------------------------------------------------------------------

goToPlayer :: Players -> PlayerId -> Result Players
goToPlayer players id = do
    (index, _) <- findPlayer players id
    return $ rotatePlayers players index

goToDealer :: Players -> Players
goToDealer players = rotatePlayers players dealerIndex
  where dealerIndex = fromMaybe 0 (findIndex ((== East) . seatWind)
                                             (getVector players))

--------------------------------------------------------------------------------

map :: (Player -> a) -> Players -> [a]
map fn players = Vec.toList $ Vec.map fn (getVector players)

head :: Players -> Player
head = Vec.head . getVector

toList :: Players -> [Player]
toList = Vec.toList . getVector

forEach :: (Player -> Player) -> Players -> Players
forEach fn players = Players $ Vec.map fn (getVector players)

filter :: (Player -> Bool) -> Players -> Players
filter fn players = Players $ Vec.filter fn (getVector players)

distributeTiles :: Int -> Tiles -> Players -> (Players, Tiles)
distributeTiles handSize tiles players =
    let (hands, remainder) = split numPlayers tiles []
     in (Players $ distributeHands hands, remainder)
  where
    distributeHands hands =
        Vec.map (\(player, hand) -> addTiles hand player)
                (Vec.zip (getVector players)
                         (Vec.fromList hands))

    split :: Int -> Tiles -> [Tiles] -> ([Tiles], Tiles)
    split _ []    hands = (hands, [])
    split 0 tiles hands = (hands, tiles)
    split n tiles hands =
        let (hand, remainder) = Prelude.splitAt handSize tiles
         in split (n-1) remainder (hand:hands)

--------------------------------------------------------------------------------
