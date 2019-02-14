import Data.Char (isSpace)
import Data.List
import Data.List.Split
import qualified Data.Map     as Map
import qualified Data.Set     as Set
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Time

data LogEntry = LogEntry  { logTime :: UTCTime
                          , logText  :: String
                          } deriving (Eq, Ord, Show)


main = do
    dataText <- fmap Text.lines (Text.readFile "4.txt")
    let dataStrings =  map Text.unpack dataText
        logEntries = sort $ map dataStringToLogEntry dataStrings
    putStrLn $ show $ map (getGuard . logText) $ filter (isGuardShift) logEntries
  

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

getGuard guardShift = read (tail $ (words guardShift)!!1)::Int

isStartSleep logEntry= (logText logEntry) == "falls asleep"

isEndSleep logEntry = (logText logEntry) == "wakes up"

arrangeLogEntries [] = [] 
arrangeLogEntries (l:ls) 
    | isGuardShift l = addGuardLog l ls
    | otherwise arrangeLogEntries ls 



-- addGuardLog g (a:b:cs)
--     | isStartSleep a  = 
--     | otherwise       = 
-- 
-- parseLogText

