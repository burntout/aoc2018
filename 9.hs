import Data.List

data Board = Board { active :: Int, placing :: [Int] } deriving (Eq, Show)
data Player = Player { playerID :: Int, score :: Int } deriving (Eq, Ord, Show)
data GameState = GameState {players :: [Player], currentPlayerID :: Int, currentBoard :: Board, currentMarble :: Int} deriving (Eq, Show)

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

updatePlayers players player newScore = unchangedPlayers ++ [updatedPlayer]
    where 
        unchangedPlayers = filter (\x -> playerID x /= player ) players
        changedPlayer = head $ filter (\x -> playerID x == player ) players
        updatedPlayer = Player { playerID = player, score = newScore + (score changedPlayer)}
    
getNextGameState game = GameState {players = updatedPlayers, currentPlayerID = nextPlayer, currentBoard = nextBoard, currentMarble = nextMarble}
    where
        playingMarble = currentMarble game
        nextMarble = 1 + playingMarble
        nextPlayer = getNextPlayer (currentPlayerID game) (players game)
        (moveScore, nextBoard) = playMarble playingMarble $ currentBoard game
        updatedPlayers = updatePlayers (players game) (currentPlayerID game) moveScore
--        
--
playGame :: Int -> Int -> GameState
playGame numPlayers maxMarble = playGame' initialGameState maxMarble
    where 
        elves = map (\x -> Player {playerID = x, score = 0}) [1 .. numPlayers]
        initBoard = Board { active = 0, placing = [0] }
        initialGameState = GameState { players = elves, currentPlayerID = 1, currentBoard = initBoard, currentMarble = 1 }

playGame' game largestMarble
    | currentMarble game > largestMarble = game
    | otherwise                 = playGame' (getNextGameState game) largestMarble


