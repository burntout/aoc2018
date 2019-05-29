-- Second go at Q11 using 
-- https://en.wikipedia.org/wiki/Summed-area_table
--

import Data.Array

-- power takes a (x,y) coordinate and generates the value of the power at that coordinate
power :: Integral b => (b, b) -> b
power p = ((((x + 10)* y) + sn) * (x + 10) `div` 100 `mod` 10) - 5 
    where
        (x, y) = p
        sn = 2694

-- values populates a 300x300 array with the power values at each (x,y) coordinate
-- Chose an array because wanted to be able to access quickly appropriate index locations
-- .. don't actually use this anywhere in this solution!
values :: Array (Integer, Integer) Integer
values = array ((1, 1),(300, 300)) [((x,y),power (x,y)) | x<-[1 .. 300], y<-[1 .. 300]]


-- psums is an array of the partial sums
-- that is, the sums of all the power values across the rectangle 
-- of which (x,y) is the lower right corner
-- To get it to work I had to wrap the arrays top and left hand sides with a row and column of zeros
psums :: Array (Integer, Integer) Integer
psums = array ((0,0),(300,300)) [((x,y), fill psums (x,y)) | x<-[0 .. 300], y<-[0 .. 300]]
    where 
        fill arr (0, _) = 0
        fill arr (_, 0) = 0
        fill arr (x,y) = power (x,y) + arr!(x, y-1) + arr!(x-1, y) - arr!(x-1, y-1)

-- this function calculates the sum of power values 
-- for squares of with a top left coordinate (x,y) and size n
-- using the algorithm from wikipedia across the partial sums array
squarePowerSums :: (Integer, Integer) -> Integer -> Integer
squarePowerSums (x,y) n  = psums!(x-1, y-1) + psums!(x+n-1, y+n-1) - psums!(x+n-1, y-1) - psums!(x-1, y+n-1)

-- given a square of size n, just range across the all possible (x,y) coordinates
-- and find the maximum, return the maximum, and the (x,y) coordinate
maxPowerSquare :: Integer -> (Integer, (Integer, Integer, Integer))
maxPowerSquare n = maximum [ (squarePowerSums (x,y) n, (x, y, n)) | x<-[1 .. 301 - n], y <- [1 .. 301 - n]]

solutionPart1 = maxPowerSquare 3

-- just range the size of the square across all possible sizes
solutionPart2 = maximum [maxPowerSquare n | n <- [1 .. 300]]

main = do
    putStrLn $ show $ solutionPart1
    putStrLn $ show $ solutionPart2
