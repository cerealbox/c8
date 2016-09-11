module Client where

import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff
import Partial.Unsafe
import Data.Either
import Data.Argonaut.Encode
import Data.Argonaut.Decode

import WS
import Game

-- --------------------------------------------------------------------------------
foreign import display :: forall e. Gamestate -> Eff (console :: CONSOLE | e) Unit

data MouseEvent = Click

foreign import data MOUSE :: !

foreign import getMouseEvent :: forall e. Eff (mouse :: MOUSE | e) MouseEvent

foreign import waitForMouseEvent :: forall eff a. 
                               Eff (mouse :: MOUSE | eff) a -> 
                               Eff (mouse :: MOUSE | eff) Unit

-- --------------------------------------------------------------------------------
uiEventLoop = do
    event <- getMouseEvent
    
    case event of
        Click -> do
            log "click!"
            send $ gEncodeJson "click"

    waitForMouseEvent uiEventLoop

socketEventLoop = do
    event <- getEvent

    case event of
        ServerMessage json -> do
            case gDecodeJson json of
                Left error -> log error
                Right gs -> display gs
        _ -> unsafeCrashWith "should never be anything other than a 'ServerMessage' as this is a client."

    waitForEvent socketEventLoop

-- --------------------------------------------------------------------------------
main :: forall e. Eff (console :: CONSOLE, mouse :: MOUSE, socket :: SOCKET | e) Unit
main = do
    connect "200.200.200.5" 8080
    waitForEvent socketEventLoop
    waitForMouseEvent uiEventLoop
