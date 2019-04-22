import Data.Array
import Data.Char
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.List.Split


data Point = Point {pos :: (Int, Int), vel :: (Int, Int)} deriving (Eq, Show)

trim = f . f
   where f = reverse . dropWhile isSpace

getPosVel inputString = Point { pos = (posX,posY), vel = (velX,velY) }
    where 
        _:x:_:v:_q = splitOneOf "<>" inputString
        [posX, posY]=  map ((\x -> read x :: Int) . trim) $ splitOn "," x 
        [velX, velY] = map ((\x -> read x :: Int) . trim) $ splitOn "," v

nextPoint p = Point { pos = (nextX, nextY), vel = vel p }
    where 
        nextX = (fst $ pos p) + (fst $ vel p)
        nextY = (snd $ pos p) + (snd $ vel p)

xRange :: [Point] -> Int
xRange ps = xMax - xMin
    where 
        xMax = maximum $ map fst $ map pos ps
        xMin = minimum $ map fst $ map pos ps

yRange ps = yMax - yMin
    where 
        yMax = maximum $ map snd $ map pos ps
        yMin = minimum $ map snd $ map pos ps

initArray ps = array ((xMin, yMin),(xMax,yMax)) [((x,y),'.') | x<-[xMin .. xMax], y<-[yMin .. yMax]]
   where
       xMax = maximum $ map fst $ map pos ps
       xMin = minimum $ map fst $ map pos ps
       yMax = maximum $ map snd $ map pos ps
       yMin = minimum $ map snd $ map pos ps

printArray a = putStr $ [ r | r <- rows] >>= (\x -> x ++ "\n")
    where
         rows = [[a!(x,y)| x <-[xmin .. xmax]] |  y <-[ymin .. ymax]]
         ((xmin,ymin),(xmax,ymax)) = bounds a

-- applyPoints arr ps 



main = do
    tls <- fmap Text.lines (Text.readFile "10.txt")
    let ss = map Text.unpack tls
        inputData = map getPosVel ss
    -- print $ elemIndex 61 $ map xRange $ take 10200 $ iterate (map nextPoint) inputData
        points = last$ take 10013 $ iterate (map nextPoint) inputData
        -- xmin = xRange points
        -- ymin = yRange points
    print $ maximum $ map snd $ map pos points 




testDataStrings = ["position=< 9,  1> velocity=< 0,  2>","position=< 7,  0> velocity=<-1,  0>","position=< 3, -2> velocity=<-1,  1>","position=< 6, 10> velocity=<-2, -1>","position=< 2, -4> velocity=< 2,  2>","position=<-6, 10> velocity=< 2, -2>","position=< 1,  8> velocity=< 1, -1>","position=< 1,  7> velocity=< 1,  0>","position=<-3, 11> velocity=< 1, -2>","position=< 7,  6> velocity=<-1, -1>","position=<-2,  3> velocity=< 1,  0>","position=<-4,  3> velocity=< 2,  0>","position=<10, -3> velocity=<-1,  1>","position=< 5, 11> velocity=< 1, -2>","position=< 4,  7> velocity=< 0, -1>","position=< 8, -2> velocity=< 0,  1>","position=<15,  0> velocity=<-2,  0>","position=< 1,  6> velocity=< 1,  0>","position=< 8,  9> velocity=< 0, -1>","position=< 3,  3> velocity=<-1,  1>","position=< 0,  5> velocity=< 0, -1>","position=<-2,  2> velocity=< 2,  0>","position=< 5, -2> velocity=< 1,  2>","position=< 1,  4> velocity=< 2,  1>","position=<-2,  7> velocity=< 2, -2>","position=< 3,  6> velocity=<-1, -1>","position=< 5,  0> velocity=< 1,  0>","position=<-6,  0> velocity=< 2,  0>","position=< 5,  9> velocity=< 1, -2>","position=<14,  7> velocity=<-2,  0>","position=<-3,  6> velocity=< 2, -1>"]

testData = map getPosVel testDataStrings
