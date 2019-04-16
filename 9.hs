{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.List.Split
import Control.Monad.State

data Board = Board { active :: Int, placing :: [Int] } deriving (Eq, Show)

-- unsafe version of elemIndex
getIndex x xs = n
    where Just n = elemIndex x xs 

-- little function to find smallest positive number == a negative number mod l
makePos x l 
    | x >= 0     = x
    | otherwise  = makePos (x + l) l


-- returns a tuple of score and a new board from an existing one using the standard rule
newb newItem board = (0, Board { active = newItem, placing = front ++ [newItem] ++ end})
    where 
        cb = placing board
        l = length cb
        current = active board
        index = getIndex current $ cb
        split = (index + 2) `mod` l 
        (front, end) = splitAt split cb

-- returns a tuple of the score and a new board from an existing one using the 23 marble rule
newb23 newItem board = (newItem + (last front), Board {active = newActive, placing = (init front) ++ end})
    where
        cb = placing board
        l = length cb
        current = active board
        index = getIndex current $ cb
        -- (subtract 6, cause indexes are from zero), or something
        split = (makePos (index - 6) l) `mod` l
        newActiveIndex = split
        newActive = cb!!newActiveIndex
        (front, end) = splitAt split cb

getNextPlayer currentPlayer players = nextPlayerID
    where 
        numPlayers = length players
        nextPlayerID = incIfZero ((currentPlayer + 1) `mod` numPlayers) numPlayers
        incIfZero x d 
           | x == 0    = x + d 
           | otherwise = x

playMarble m b 
    | m `mod` 23 == 0  = newb23 m b
    | otherwise        = newb m b


playGame []     = do
    (scores, _) <- get
    return scores

playGame (x:xs) = do
    (scores, board) <- get
    let (score, newBoard) =  playMarble x board
    put (scores ++ [score], newBoard)
    playGame xs


game numPlayers maxMarble = map sum $ transpose chunked 
    where 
        startState = ([], Board { active = 0, placing = [0] })
        result = evalState (playGame [1 .. maxMarble])  startState
        chunked = chunksOf numPlayers result

main = print $ maximum $ game 13 7999
