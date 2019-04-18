{-# LANGUAGE FlexibleContexts #-}

import Data.CircularList
import qualified Data.Map as Map
import Data.List
import Data.List.Split
import Control.Monad.State

-- unsafe version of Clist focus
getFocus cl = n
    where Just n = focus cl

-- returns a tuple of score and a new board from an existing one using the standard rule
newb newMarble board = (0, newBoard)
    where 
        newBoard = insertL newMarble $ rotR board

-- returns a tuple of the score and a new board from an existing one using the 23 marble rule
newb23 newMarble board = (newMarble + removedMarble, newBoard)
    where
        tmpBoard = rotN (-7) board
        removedMarble  = getFocus tmpBoard
        newBoard = removeR tmpBoard

-- play a marble, just picks the appropriate rule depending on the marble
playMarble m b 
    | m `mod` 23 == 0  = newb23 m b
    | otherwise        = newb m b


-- If we have no marbles, then there is nothing to do so
-- just return the player scores from the game state.
playGame _ []     = do
    (scores, _) <- get
    return scores

-- play the game.  Pop a marble from the list of marbles
-- get the current state of scores and board
-- play the marble and alter the state
-- the ... do it again, but with the remaining marbles
playGame numPlayers (x:xs) = do
    (scores, board) <- get
    let (score, newBoard) =  playMarble x board
    let player = x `mod` numPlayers
    -- only update the scores if we have to
    -- makes a big difference to running time
    case score of 
        0 -> put (scores, newBoard)
        _ -> put (Map.insertWith (+) player score scores, newBoard)
    playGame numPlayers xs

-- initialise the game scores as a Map
-- and the board as a CList for easy insertion and moving etc
game numPlayers maxMarble = result
    where 
        initScores = Map.fromList []
        startState = (initScores, fromList [0])
        result = evalState (playGame numPlayers [1 .. maxMarble]) startState

main = print $ maximum $ game 411 7205900
