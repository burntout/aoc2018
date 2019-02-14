import Data.Char
import Data.List.Split
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    pointsText <- fmap Text.lines (Text.readFile "6.txt")
    let pointStrings = map Text.unpack pointsText
        points = map stringToPoint pointStrings 
        allPointsToMeasure = getAllPointsToMeasure points
        listOfClosestPoints = map (findClosestPoint points) allPointsToMeasure
        getCountsOfClosestPoints = maximum [length $ filter (==(Just p)) listOfClosestPoints | p <- points]
    putStrLn $ show $ getCountsOfClosestPoints

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

getAllPointsToMeasure points = [ Point { xcoord = x, ycoord = y } | x <- [minX  .. maxX], y <- [minY .. maxY]]
    where 
        minX = minimum $ map xcoord points
        minY = minimum $ map ycoord points
        maxX = maximum $ map xcoord points
        maxY = maximum $ map ycoord points

findClosestPoint :: [Point]-> Point -> Maybe Point
findClosestPoint ps p
    | length closestPoints == 1  = Just $ head closestPoints
    | otherwise                   = Nothing
    where 
        distanceToPoints = map (\x -> ((distance x p), x)) ps
        minDistance = minimum $ map fst distanceToPoints
        closestPoints = map snd $ filter (\x -> fst x == minDistance) distanceToPoints
      





