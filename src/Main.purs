module Main where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Data.List 
import Data.Foldable (foldl, foldr, sum)
import Control.Monad.Eff.Random (randomInt, RANDOM)
import Control.MonadZero (guard)
import Data.Maybe
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Data.Boolean

import Game

--import Signal (Signal, (~>), runSignal, constant)
--import Signal.Time


-- --------------------------------------------------------------------------------
foreign import data TIMEOUT :: !
foreign import timeout :: forall eff a. 
                               Int -> 
                               Eff (timeout :: TIMEOUT | eff) a -> 
                               Eff (timeout :: TIMEOUT | eff) Unit

foreign import data CLICK :: !
foreign import click :: forall eff a. 
                               Eff (click :: CLICK | eff) a -> 
                               Eff (click :: CLICK | eff) Unit

foreign import display :: Gamestate -> Unit


-- --------------------------------------------------------------------------------
gameLoop gs@(Gamestate {done: true}) = do
    log " -++- new game -++- "
    ngs <- update gs NewGame
    log $ show ngs
    let __ = display ngs
    
    --click $ gameLoop ngs
    timeout 2000 $ gameLoop ngs

gameLoop gs = do
    log $ showPlay gs (ai gs)
    ngs <- update gs $ ai gs
    
    log $ show ngs
    let __ = display ngs

    --click $ gameLoop ngs
    timeout 1000 $ gameLoop ngs


-- --------------------------------------------------------------------------------
emptyGameState = Gamestate { 
    players : Nil, 
    deck : Nil, 
    discard : Nil,
    perCardScore : 1,
    alreadyDrew : false,
    done: true
}

-- --------------------------------------------------------------------------------
main :: forall e. Eff (console :: CONSOLE, random :: RANDOM, timeout :: TIMEOUT, click :: CLICK | e) Unit
main = do
    let a = emptyGameState
    b <- update a $ AddPlayer "erik"
    c <- update b $ AddPlayer "jafar"
    gameLoop c


-- --------------------------------------------------------------------------------
--hello = constant "Hello Joe!"

--helloEffect :: forall eff. Signal (Eff (console :: CONSOLE | eff) Unit)
--helloEffect = hello ~> log

--hi = delay 1000.0 hello
--main = runSignal hi
