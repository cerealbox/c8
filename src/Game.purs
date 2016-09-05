module Game where

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

-- --------------------------------------------------------------------------------
infixr 0 mod as %

pipe a b = b a
infixr 0 pipe as |>

-- --------------------------------------------------------------------------------
data Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
data Suit = Clubs | Spades | Hearts | Diamonds
data Card = Card Face Suit
data CardView = CardView Card Suit
newtype Player = Player { hand :: List Card, score :: Int, name :: String, id :: Int }
newtype Gamestate = Gamestate { 
    players :: List Player,  
    deck :: List Card, 
    discard :: List CardView,
    perCardScore :: Int,
    alreadyDrew :: Boolean,
    done :: Boolean
}
data Action = Play (List CardView) | NewGame | AddPlayer String


-- --------------------------------------------------------------------------------
toCard (CardView c _) = c
mkCardView (Card f s) = CardView (Card f s) s

instance faceEq :: Eq Face where 
    eq Ace Ace = true
    eq Two Two = true
    eq Three Three = true
    eq Four Four = true
    eq Five Five = true
    eq Six Six = true
    eq Seven Seven = true
    eq Eight Eight = true
    eq Nine Nine = true
    eq Ten Ten = true
    eq Jack Jack = true
    eq Queen Queen = true
    eq King King = true
    eq _ _ = false

instance suitEq :: Eq Suit where 
    eq Clubs Clubs = true
    eq Spades Spades = true
    eq Hearts Hearts = true
    eq Diamonds Diamonds = true
    eq _ _ = false

instance cardEq :: Eq Card where 
    eq (Card f1 s1) (Card f2 s2) = f1 == f2 && s1 == s2


-- --------------------------------------------------------------------------------
--TODO: gameStateShow only properly shows a game with 2 players.
instance gameStateShow :: Show Gamestate where
    show (Gamestate {deck, discard, players, perCardScore}) = 
        case players of
            Player {id} : _ ->
                case filter (\(Player x) -> x.id == 0) players of
                    Player player0 : _ -> 
                        case filter (\(Player x) -> x.id == 1) players of
                            Player player1 : _ ->
                                "--------------------"
                                <> "\n{" <> show player0.score <> "} player0: " <> showHand player0.hand
                                <> "\nX" <> show perCardScore <> " deck: " <> show (length deck) <> " cards\npile: " <> showHand discard
                                <> "\n{" <> show player1.score <> "} player1: " <> showHand player1.hand
                                <> "\n--------------------\n"
                            _ -> unsafeCrashWith "no player 1."
                    _ -> unsafeCrashWith "no player 0.."
            _ -> unsafeCrashWith "no player 0."

showHand x = showHand'' $ reverse x
showHand'' (head : Nil) = "[" <> show head <> "]"
showHand'' (head : tail) = 
    "[" <> show head <> " " <> showHand' tail
    where
    showHand' (head : Nil) = show head <> "]"
    showHand' (head : tail) = show head <> " " <> showHand' tail
    showHand' Nil = "]"
showHand'' Nil = "[]"

showPlay gs play =
    case gs of
        Gamestate {players: (Player {id}) : _} ->
            case play of
                Play cards -> 
                    "\n** player " <> show id <> "'s' move: " <> (showHand $ map toCard cards)
                _ -> unsafeCrashWith "this should never happen"
        _ -> unsafeCrashWith "this should never happen"


-- --------------------------------------------------------------------------------
instance cardViewShow :: Show CardView where
    show (CardView (Card f _) s) = "@" <> show (Card f s)

instance cardShow :: Show (Card) where 
    show (Card f s) = show f <> show s

instance playerShow :: Show (Player) where
    show (Player {hand, score}) = "[" <> show hand <> "*" <> show score <> "]"

instance faceShow :: Show (Face) where
    show Ace = "A"
    show Jack = "J"
    show King = "K"
    show Queen = "Q"
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "0"

instance suitShow :: Show (Suit) where
    show Clubs = "♣"
    show Spades = "♠"
    show Hearts = "♥"
    show Diamonds = "♦"


-- --------------------------------------------------------------------------------
shuffle =
    foldM (\acc cur -> do
        a <- randomInt 0 1
        pure $ case a of
            0 -> cur : acc
            _ -> acc <> cur : Nil
    ) Nil

allCards = do
    suit <- foldr Cons Nil [Clubs, Spades, Hearts, Diamonds]
    face <- foldr Cons Nil [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
    pure $ Card face suit


-- --------------------------------------------------------------------------------
reshuffle (Gamestate gs) = 
    case gs.discard of
        CardView (Card topf _) _ : tail -> do
            deck' <- (dropWhile (sameFace topf)) >>> (map toCard) >>> shuffle $ gs.discard
            pure $ Gamestate gs {
                    deck = deck', 
                    discard = takeWhile (sameFace topf) gs.discard
                }
        Nil -> unsafeCrashWith "reshuffle called on empty discard pile!"
    where
        sameFace face (CardView (Card f _) _) = f == face


-- --------------------------------------------------------------------------------
pickup 0 gs = pure gs
pickup n gameState@(Gamestate gs) = 
    case gs.deck of
        Nil -> do
            gameState' <- reshuffle gameState
            pickup n gameState'
        head : tail -> do
            pickup 
                (n - 1) $
                Gamestate gs { 
                    players = activePlayerPickup gs.players head, 
                    deck = tail 
                }
    where
        activePlayerPickup ((Player p) : rest) card = Player p {hand = card : p.hand} : rest
        activePlayerPickup Nil card = unsafeCrashWith "no players."


-- --------------------------------------------------------------------------------
burn (Card Queen Spades : _) gameState = pickup 5 gameState
burn (Card Two _ : _) gameState@(Gamestate {discard}) =
    pickup (length twos * 2) gameState
    where
    twos = takeWhile (\(CardView (Card f _) _) -> f == Two) discard
burn _ gameState = pure gameState


-- --------------------------------------------------------------------------------
--TODO: how do ties work for more than 2 players?
checkForWinnerAndUpdateScores gameState@(Gamestate gs) =
    case gs.players of
        Player { hand: Nil } : Player { hand: Nil } : Nil ->
            Gamestate gs {
                done = true,
                perCardScore = gs.perCardScore * 2
            }
        Player winner@{ hand: Nil } : losers ->
            let points = sum $ map (\(Player {hand}) -> length hand ) losers in
            Gamestate gs {
                done = true, 
                players = updateScore points winner : losers,
                perCardScore = 1
            }
        _ -> gameState
    where
    updateScore score p = Player p {score = p.score + (score * gs.perCardScore) }

--TODO: playing multiple jack's probably does not work correctly with more than 2 playres atm.
--      (we are only checking if the top card of a play is a jack)
--TODO: what does an ace do with only 2 players?
handlePlay (Gamestate gs) playCardViews = do
    let play = map toCard playCardViews
    case play of
        Nil -> 
            case gs.alreadyDrew of
                true -> pure $ Gamestate $ nextTurn $ gs { alreadyDrew = false }
                false -> pickup 1 $ Gamestate gs {alreadyDrew = true }
        Card Jack _ : _ -> pure $ Gamestate $ nextTurn $ nextTurn $ playHand gs
        Card Ace _ : _ -> pure $ Gamestate $ reverseTurn $ playHand gs
        _ -> burn play $ Gamestate $ nextTurn $ playHand gs
    where
        playHand gs@{players: (Player player) : rest} = gs {
            discard = playCardViews <> gs.discard,
            alreadyDrew = false,
            players = Player (removeCards player) : rest
        }
        playHand {players: Nil} = unsafeCrashWith "no players."
        removeCards p = p {hand = p.hand \\ (map toCard playCardViews) }

        nextTurn gs@{players: (player : rest)} = gs {players = rest <> player : Nil}
        nextTurn {players: Nil} = unsafeCrashWith "no players."

        reverseTurn gs@{players} = gs {players = reverse players}
        reverseTurn {players: Nil} = unsafeCrashWith "no players."

-- --------------------------------------------------------------------------------
update gameState (Play playCardViews) = do
    gameState' <- handlePlay gameState playCardViews
    pure $ checkForWinnerAndUpdateScores gameState'

update (Gamestate gs) NewGame = do
    shuffledCards <- shuffle allCards
    case shuffledCards of
        topCard@(Card _ suit) : deck ->
            pure $ Gamestate $ 
                foldr 
                    (\(Player player) (gs@{players, deck})  -> 
                        gs {
                            players = Player player {
                                hand = take 8 deck
                            } : players,
                            deck = drop 8 deck
                        })
                    gs {
                        players = Nil, 
                        deck = deck, 
                        discard = CardView topCard suit : Nil,
                        done = false
                    }
                    gs.players
        _ -> unsafeCrashWith "failed to initialize cards."

update (Gamestate gs) (AddPlayer name) = 
    pure $ Gamestate gs {
        players = Player {hand: Nil, score: 0, name: name, id: length gs.players} : gs.players
    }

-- --------------------------------------------------------------------------------
--validPlay? :: List CardView -> Gamestate -> Boolean

-- --------------------------------------------------------------------------------
ai (Gamestate {players: Player currentPlayer : _, discard: topCard: _}) =
    case possiblePlays of
        play : _ -> Play $ map mkCardView play
        Nil -> Play Nil
    where
    playableCards = filter (canPlay topCard) currentPlayer.hand
    possiblePlays = do
        card@(Card f _) <- playableCards
        let matchingCards = filter (\(Card f2 _) -> f == f2) currentPlayer.hand
        pure $ (matchingCards \\ card : Nil) <> (card : Nil)
    canPlay _ (Card Eight _) = true
    canPlay (CardView (Card topF _) topS) (Card f s) = f == topF || s == topS

ai gameState@(Gamestate {players: _, discard: _}) = Play Nil
