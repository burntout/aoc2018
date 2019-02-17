import Control.Monad.State
import Data.Char (isSpace)
import Data.List
import Data.List.Split
import qualified Data.Map     as Map
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Time

data LogEntry = LogEntry  { logTime :: UTCTime
                          , logText  :: String
                          } deriving (Eq, Ord, Show)


main = do
    dataText <- fmap Text.lines (Text.readFile "4.txt")
    let dataStrings =  map Text.unpack dataText

        -- Parse the input and turn into LogEntry types
        -- Process the the logs using the Control.Monad.State
        -- This outputs a Map keyed by Guard
        --
        logEntries = sort $ map dataStringToLogEntry dataStrings
        startState = (0, Map.empty)
        logsByGuard = evalState (processLogs logEntries) startState

        -- get a list of the guards
        guards = Map.keys logsByGuard
        
        -- Lookup the times associated with that guard
        times g = Map.lookup g logsByGuard
        -- Lookup returns a Maybe so we need to use a bind to process the results 
        sleepGuard g = times g >>= return . sleepLength
        laziestGuard = snd $ maximum $ map (\x -> (sleepGuard x, x)) guards

    print laziestGuard
  
dataStringToLogEntry str = LogEntry {logTime = logTime, logText = logText}
    where 
        [timestamp, logText] = map trim $ splitOn "]" $ splitOn "[" str >>= id
        [dateStr, timeStr] = splitOn " " timestamp
        datePart = map (\x -> read x ::Int) $ splitOn "-" dateStr
        day = fromGregorian (toInteger (datePart!!0)) (datePart!!1) (datePart!!2)
        timePart = map (\x -> read x :: Integer) $ splitOn ":" timeStr
        seconds = (60 * 60 * timePart!!0) + 60 * timePart!!1
        logTime = UTCTime day $ fromInteger seconds
        
trim = f . f
   where f = reverse . dropWhile isSpace

isGuardShift logEntry = drop (length str - 12) str == "begins shift"
    where
        str = logText logEntry

getGuard logEntry = read (tail $ (words guardShift)!!1)::Int
    where guardShift = logText logEntry 

isStartSleep logEntry= (logText logEntry) == "falls asleep"

isEndSleep logEntry = (logText logEntry) == "wakes up"

appendLogEntry id entry dict = Map.insertWith (++) id entry dict

-- Process the logEntries
-- Used the following 
-- https://wiki.haskell.org/State_Monad#Complete_and_Concrete_Example_1

processLogs [] = do 
    (_, guardTimes) <- get
    return guardTimes 

processLogs (x:xs) = do
    (guardID, guardTimes) <- get
    case (logType x) of
        "guard" -> put (getGuard x, guardTimes)
        "sleep" -> put (guardID, appendLogEntry guardID [(logTime x,0)] guardTimes)
        "wakes" -> put (guardID, appendLogEntry guardID [(logTime x,1)] guardTimes)
        _       -> put (guardID, guardTimes)
    processLogs xs

logType x
    | isGuardShift x = "guard"
    | isStartSleep x = "sleep"
    | isEndSleep x   = "wakes"

startSleep times = map fst $ filter (\x-> snd x == 0) times
endSleep times = map fst $ filter (\x-> snd x == 1) times
sleepLength times = sum $ zipWith (diffUTCTime) (endSleep times) (startSleep times)

testData = ["[1518-11-01 00:00] Guard #10 begins shift","[1518-11-01 00:05] falls asleep","[1518-11-01 00:25] wakes up","[1518-11-01 00:30] falls asleep","[1518-11-01 00:55] wakes up","[1518-11-01 23:58] Guard #99 begins shift","[1518-11-02 00:40] falls asleep","[1518-11-02 00:50] wakes up","[1518-11-03 00:05] Guard #10 begins shift","[1518-11-03 00:24] falls asleep","[1518-11-03 00:29] wakes up","[1518-11-04 00:02] Guard #99 begins shift","[1518-11-04 00:36] falls asleep","[1518-11-04 00:46] wakes up","[1518-11-05 00:03] Guard #99 begins shift","[1518-11-05 00:45] falls asleep","[1518-11-05 00:55] wakes up"]
