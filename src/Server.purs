module Server where

import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff
import Data.List 
import Data.Foldable
import Control.Monad.Eff.Random
import Partial.Unsafe
import Data.Either
import Data.Argonaut.Encode
import Data.Argonaut.Decode

import Game
import WS

-- --------------------------------------------------------------------------------
gameLoop gs@(Gamestate {done: true}) = do
    log " -++- new game -++- "
    ngs <- update gs NewGame
    log $ show ngs
    sendToAll $ gEncodeJson ngs
    gameLoop ngs

gameLoop gs = do

    event <- getEvent --WARNING: infinite loop if you never 'getEvent' in 'waitForEvent'?
    
    case event of
        Connection (Client ip port) -> do
            log $ "connect: " <> ip <> ":" <> show port

            ngs <- update gs $ AddPlayer $ ip <> ":" <> show port
            sendToAll $ gEncodeJson ngs

            waitForEvent $ gameLoop ngs

        ClientMessage (Client ip port) json -> do
            log $ ip <> show port <> ": " <> show json

            case gs of
                Gamestate {players: (Player {ipport}) : _} | ipport == (ip <> ":" <> show port) -> do
                    ngs <- update gs $ ai gs
                    sendToAll $ gEncodeJson ngs
                    waitForEvent $ gameLoop ngs

                _ -> waitForEvent $ gameLoop gs

        Close (Client ip port) -> do
            log $ "close: " <> ip <> ":" <> show port

            ngs <- update gs $ RemovePlayer $ ip <> ":" <> show port
            sendToAll $ gEncodeJson ngs

            waitForEvent $ gameLoop ngs

        Tick ->
            case gs of
                Gamestate {players: (Player {ai: true}) : _} -> do
                    log $ showPlay gs (ai gs)
                    ngs <- update gs $ ai gs
                    log $ show ngs
                    sendToAll $ gEncodeJson ngs
                    waitForEvent $ gameLoop ngs
                
                _ -> waitForEvent $ gameLoop gs

        _ -> unsafeCrashWith "failure."

-- --------------------------------------------------------------------------------
main :: forall e. Eff (console :: CONSOLE, random :: RANDOM, socket :: SOCKET | e) Unit
main = do

    x <- shuffle allCards
    log $ showHand $ sort x
    log "\n"

    listen "200.200.200.5" 8080
    waitForEvent $ gameLoop emptyGameState
