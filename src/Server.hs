module Server (runServer) where

import Server.Room

import Data.Map as Map
import Data.MiniMap as Mini

import Control.Result

import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy

import qualified Data.String as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS

import Data.Text as Text
import Data.Text.Encoding (decodeUtf8)

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

--------------------------------------------------------------------------------

runServer :: Int -> IO ()
runServer port = withSocketsDo $ do
    let hints = defaultHints {
        addrFlags = [AI_PASSIVE]
      , addrSocketType = Stream
    }

    addr <- Prelude.head <$> getAddrInfo (Just hints) Nothing (Just (show port))

    rooms <- newMVar Map.empty

    bracket (openSocket addr) (flip gracefulClose 2) $ \sock -> do
        withFdSocket sock setCloseOnExecIfNeeded
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 4

        putStrLn ("Mahjong Server is listening on " <> show port)

        forever $ accept sock >>= \(conn, addr) -> do
            forkIO $ finally (notifyJoin addr >> clientInit rooms conn addr)
                             (notifyExit addr >> close conn)
  where
    notifyJoin addr = putStrLn (show addr <> " has joined.")
    notifyExit addr = putStrLn (show addr <> " has left.")

--------------------------------------------------------------------------------

data ClientControl = Start RoomID | Retry | Exit Text

{-
    A -> SERVER : new
    SERVER -> A : OK 'ABCD'
    A -> SERVER : start
    SERVER -> A : ERR Need 4 Players
    B -> SERVER : join 'ABCD'
    SERVER -> * : OK [A, B]
    C -> SERVER : join 'ABCD'
    SERVER -> * : OK [A, B, C]
    D -> SERVER : join 'ABCD'
    SERVER -> * : OK [A, B, C, D]
    A -> SERVER : start
    SERVER -> * : OK
-}

clientInit :: MVar Rooms -> Socket -> SockAddr -> IO ()
clientInit rooms sock addr = fix $ \loop -> do
    msg <- recv sock 1024

    let args = Text.words $ decodeUtf8 msg

    case attemptInit args of
        Start _ -> pure ()
        Retry   -> pure ()
        Exit e  -> putStrLn $ show e

  where
    attemptInit :: [Text] -> ClientControl
    attemptInit words = Exit "Test"

--------------------------------------------------------------------------------
