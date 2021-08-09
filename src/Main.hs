module Main where

import Server

main :: IO ()
main = runServer 9090

{-
main = do
    idents  <- replicateM 4 $ newPlayer "A"

    putStrLn $ prettyShow $ idents

    board   <- return $ unwrap $ newBoard idents
    board   <- startRound False board
    board   <- return $ unwrap board

    putStrLn $ prettyShow $ board

    args    <- getLine >>= return . pack

    let minival = fromJust (toMap (fromJust (parse args)))

    board'  <- return $ doAction board (playerId $ idents !! 2) minival

    putStrLn $ prettyShow $ board'

{-
    board <- startRound True board
    board <- unwrap board

    Prelude.mapM (putStrLn . show)
               $ Players.map (Players.concealedTiles)
                             (Board.players board)

    putStrLn $ prettyShow $ Players.map testPlayer (Board.players board)

  where
    unwrap :: Result a -> IO a
    unwrap = return . Control.Result.unwrap

    testPlayer player =  testForMelds (Discarded (Dots 5))
                                      (position player)
                                      (getHand player)
-}
-}
