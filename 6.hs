import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    pointsText <- fmap Text.lines (Text.readFile "6.txt")
    let pointStrings = map Text.unpack pointsText

        fixedPoints = map stringToPoint pointStrings -- Parse the input file, and generate a list of Points 
        allPointsToMeasure = getAllPointsToMeasure fixedPoints -- Generate a list of points in the smallest bounding box 
        allBoundaryPointsToMeasure = getBoundaryPoints fixedPoints -- Generate a list of points that form the boundary of the box

        -- Find all the fixed points that are closest to the boundary 
        -- These fixed points will generate infinite areas
        pointsThatGoToInfinity = nub $ map (findClosestPoint fixedPoints) allBoundaryPointsToMeasure

        -- For each point in the bounding box, find the closest fixed point
        listOfClosestPoints = map (findClosestPoint fixedPoints) allPointsToMeasure

        -- remove the points closest to a fixedPoint that will go to infinity
        -- Leaving the fixed points that will have a finite area
        finitePoints = removeElements listOfClosestPoints pointsThatGoToInfinity

        -- Finally calculate the area of regions, by counting 
        getCountsOfClosestPoints = maximum [length $ filter (==(Just p)) finitePoints | p <- fixedPoints]

    putStrLn $ show $ getCountsOfClosestPoints


-- Type to hold points in
-- and some helpers to parse the input
data Point = Point { xcoord :: Int, ycoord :: Int } deriving (Eq, Show)

listToPoint :: [Int] -> Point
listToPoint [a,b] = Point { xcoord = a, ycoord = b}

stringToList :: String -> [Int]
stringToList str = map (\x -> read x :: Int) $ splitOn ", " str

stringToPoint :: String -> Point
stringToPoint = listToPoint . stringToList

-- Manahattan Distance metric 
distance :: Point -> Point -> Int
distance p1 p2 = xdiff + ydiff
    where
        xdiff = abs $ (xcoord  p1) - (xcoord p2)
        ydiff = abs $ (ycoord  p1) - (ycoord p2)

-- Get all Points that lie within the smallest bounding box containing the fixed points
getAllPointsToMeasure points = [Point { xcoord = x, ycoord = y } | x <- [minX  .. maxX], y <- [minY .. maxY]]
    where 
        minX = minimum $ map xcoord points
        minY = minimum $ map ycoord points
        maxX = maximum $ map xcoord points
        maxY = maximum $ map ycoord points

-- Get the points that form the boundary of the bounding box
getBoundaryPoints points = r1 ++ r2 ++ r3 ++ r4
    where 
        minX = minimum $ map xcoord points
        minY = minimum $ map ycoord points
        maxX = maximum $ map xcoord points
        maxY = maximum $ map ycoord points
        r1 = [Point { xcoord = x, ycoord = minY } | x <- [minX .. maxX - 1]] 
        r2 = [Point { xcoord = x, ycoord = maxY } | x <- [minX + 1 .. maxX]] 
        r3 = [Point { xcoord = minX, ycoord = y } | y <- [minY + 1 .. maxY]] 
        r4 = [Point { xcoord = maxX, ycoord = y } | y <- [minY .. maxY - 1]] 

-- Given a list of fixed points and an arbitrary point return the unique fixed point closest, or Nothing if there is no unique one
findClosestPoint :: [Point]-> Point -> Maybe Point
findClosestPoint ps p
    | length closestPoints == 1  = Just $ head closestPoints
    | otherwise                   = Nothing
    where 
        distanceToPoints = map (\x -> ((distance x p), x)) ps
        minDistance = minimum $ map fst distanceToPoints
        closestPoints = map snd $ filter (\x -> fst x == minDistance) distanceToPoints
      
-- Function to remove a list of elements from another list 
removeElements :: (Eq a) => [a] -> [a] -> [a]
removeElements haves nots = [h | h <- haves, (h `elem` nots) == False]
