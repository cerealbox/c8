module Main where

import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff
import Control.Monad.Eff.Random

import Game

-- --------------------------------------------------------------------------------
foreign import data TIMEOUT :: !
foreign import timeout :: forall eff a. 
                               Int -> 
                               Eff (timeout :: TIMEOUT | eff) a -> 
                               Eff (timeout :: TIMEOUT | eff) Unit

foreign import display :: forall e. Gamestate -> Eff (timeout :: TIMEOUT | e) Unit

-- --------------------------------------------------------------------------------
gameLoop gs@(Gamestate {done: true}) = do
    log " -++- new game -++- "
    ngs <- update gs NewGame
    log $ show ngs
    display ngs
    
    timeout 2000 $ gameLoop ngs

gameLoop gs = do
    log $ showPlay gs (ai gs)
    ngs <- update gs $ ai gs
    
    log $ show ngs
    display ngs

    timeout 1000 $ gameLoop ngs


-- --------------------------------------------------------------------------------
main :: forall e. Eff (console :: CONSOLE, random :: RANDOM, timeout :: TIMEOUT | e) Unit
main = do
    gameLoop emptyGameState
