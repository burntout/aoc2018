import Data.Char
import Data.List.Split
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    pointsText <- fmap Text.lines (Text.readFile "6.txt")
    let pointStrings = map Text.unpack pointsText
        points = map stringToPoint pointStrings 
    putStrLn $ show $ points

data Point = Point { xcoord :: Int, ycoord :: Int } deriving (Eq, Show)

listToPoint [a,b] = Point { xcoord = a, ycoord = b}

stringToList :: String -> [Int]
stringToList str = map (\x -> read x :: Int) $ splitOn ", " str

stringToPoint = listToPoint . stringToList

distance :: Point -> Point -> Int
distance p1 p2 = xdiff + ydiff
    where
        xdiff = abs $ (xcoord  p1) - (xcoord p2)
        ydiff = abs $ (ycoord  p1) - (ycoord p2)
