import Data.List

data Board = Board { active :: Int, placing :: [Int] } deriving (Eq, Show)
data Player = Player { playerID :: Int, score :: Int } deriving (Eq, Show)
data GameState = GameState {players :: [Player], currentPlayerID :: Player, currentBoard :: Board, availableMarbles :: [Int]} deriving (Eq, Show)

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
        newActiveIndex = (split + 1) `mod` l
        newActive = cb!!newActiveIndex
        (front, end) = splitAt split cb

getNextPlayer currentPlayer players = head $ filter (\x -> playerID x == nextPlayerID ) players
    where 
        numPlayers = length players
        nextPlayerID = incIfZero (((playerID currentPlayer) + 1) `mod` numPlayers) numPlayers
        incIfZero x d 
           | x == 0    = x + d 
           | otherwise = x

playMarble m b 
    | m `mod` 23 == 0  = newb23 m b
    | otherwise        = newb m b

updatePlayers players player newScore = unchangedPlayers ++ [updatedPlayer]
    where 
        unchangedPlayers = filter (\x -> playerID x /= playerID player ) players
        updatedPlayer = Player { playerID = playerID player, score = newScore + (score player)}
    
getNextGameState game = GameState {players = updatedPlayers, currentPlayer = nextPlayer, currentBoard = nextBoard, availableMarbles = remainingMarbles}
    where
        playingMarble = head $ availableMarbles game
        remainingMarbles = tail $ availableMarbles game
        nextPlayer = getNextPlayer (currentPlayer game) (players game)
        (moveScore, nextBoard) = playMarble playingMarble $ currentBoard game
        updatedPlayers = updatePlayers (players game) (currentPlayer game) moveScore
        

playGame game
    | availableMarbles game == [] = game
    | otherwise                 = playGame $ getNextGameState game

elves = map (\x -> Player {playerID = x, score = 0}) [1 .. 411]
initPlayer = head $ filter (\x -> playerID x == 1 ) elves
initBoard = Board { active = 0, placing = [0] }
initMarbles = [1 .. 72059]

initialGameState = GameState { players = elves, currentPlayer = initPlayer, currentBoard = initBoard, availableMarbles = initMarbles }


