module WS where

import Prelude
import Control.Monad.Eff
import Data.Argonaut.Core

import Game

-- --------------------------------------------------------------------------------

data Client = Client String Int | Wubalubadubdub
data SocketEvent = Connection Client | ClientMessage Client Json | Close Client | ServerMessage Json |Tick

foreign import data SOCKET :: !

foreign import waitForEvent :: forall eff a. 
                               Eff (socket :: SOCKET | eff) a -> 
                               Eff (socket :: SOCKET | eff) Unit

foreign import listen :: forall e. String -> Int -> Eff (socket :: SOCKET | e) Unit
foreign import listenStatic :: forall e. String -> Int -> Eff (socket :: SOCKET | e) Unit
foreign import getEvent :: forall e. Eff (socket :: SOCKET | e) SocketEvent
foreign import sendToAll :: forall e. Json -> Eff (socket :: SOCKET | e) Unit
foreign import send :: forall e. Json -> Eff (socket :: SOCKET | e) Unit
foreign import connect :: forall e. String -> Int -> Eff (socket :: SOCKET | e) Unit
