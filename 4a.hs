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
        -- laziestGuard = snd $ maximum $ map (\x -> (sleepGuard x, x)) guards
        -- laziestMinute = times laziestGuard >>= return . maxFreq . frequencies . dateToMin 
        -- solution = laziestMinute >>= return . (* laziestGuard)
        solution = Map.map maxFreq $ Map.map frequencies $ Map.map dateToMin logsByGuard

    print solution
  
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

getGuard logEntry = read (tail $ (words guardShift)!!1)::Integer
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

-- startSleep :: [(UTCTime,Int)] -> [UTCTime]
startSleep times = map fst $ filter (\x-> snd x == 0) times

-- endSleep :: [(UTCTime,Int)] -> [UTCTime]
endSleep times = map fst $ filter (\x-> snd x == 1) times

-- sleepLength :: [(UTCTime, Int)] -> NominalDiffTime
sleepLength times = sum $ zipWith (diffUTCTime) (endSleep times) (startSleep times)

getMinuteOfTime :: UTCTime -> Integer
getMinuteOfTime t = m
    where 
        UTCTime day seconds = t
        m = (diffTimeToPicoseconds seconds) `quot` (60 * 10^12)

-- dateToMin :: [(UTCTime, Int)] -> [Integer]
dateToMin times = zipWith (\x y -> [x .. y-1]) sleepStartMins sleepEndMins >>= id
    where
        sleepStartMins = map getMinuteOfTime $ startSleep times
        sleepEndMins  = map getMinuteOfTime $ endSleep times

-- frequencies :: [a] -> Map a Integer
frequencies []     = Map.empty
frequencies (a:as) = Map.insert a (length (filter (==a) (a:as))) (frequencies $ filter (/= a) as)

maxFreq  x = fst . head $ Map.toList $ Map.filter ( == maximum x) x

testData = ["[1518-11-01 00:00] Guard #10 begins shift","[1518-11-01 00:05] falls asleep","[1518-11-01 00:25] wakes up","[1518-11-01 00:30] falls asleep","[1518-11-01 00:55] wakes up","[1518-11-01 23:58] Guard #99 begins shift","[1518-11-02 00:40] falls asleep","[1518-11-02 00:50] wakes up","[1518-11-03 00:05] Guard #10 begins shift","[1518-11-03 00:24] falls asleep","[1518-11-03 00:29] wakes up","[1518-11-04 00:02] Guard #99 begins shift","[1518-11-04 00:36] falls asleep","[1518-11-04 00:46] wakes up","[1518-11-05 00:03] Guard #99 begins shift","[1518-11-05 00:45] falls asleep","[1518-11-05 00:55] wakes up"]

testLogEntries = map dataStringToLogEntry testData
startState = (0, Map.empty)
testLogsByGuard = evalState (processLogs testLogEntries) startState
testGuards = Map.keys testLogsByGuard
times g = Map.lookup g testLogsByGuard
testSol = Map.map maxFreq $ Map.map frequencies $ Map.map dateToMin testLogsByGuard
