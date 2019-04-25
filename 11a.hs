-- Second go at Q11 using 
-- https://en.wikipedia.org/wiki/Summed-area_table
--

import Data.Array

power p = ((((x + 10)* y) + sn) * (x + 10) `div` 100 `mod` 10) - 5 
    where
        x = fst p
        y = snd p
        sn = 2694

values = array ((1, 1),(300, 300)) [((x,y),power (x,y)) | x<-[1 .. 300], y<-[1 .. 300]]

psums = array ((0,0),(300,300)) [((x,y), fill psums (x,y)) | x<-[0 .. 300], y<-[0 .. 300]]
    where 
        fill arr (0, _) = 0
        fill arr (_, 0) = 0
        fill arr (x,y) = power (x,y) + arr!(x, y-1) + arr!(x-1, y) - arr!(x-1, y-1)

squarePowerSums (x,y) n  = psums!(x-1, y-1) + psums!(x+n-1, y+n-1) - psums!(x+n-1, y-1) - psums!(x-1, y+n-1)

maxPowerSquare n = maximum [ (squarePowerSums (x,y) n, (x, y, n)) | x<-[1 .. 301 - n], y <- [1 .. 301 - n]]

maxPowerAllSquares = maximum [maxPowerSquare n | n <- [1..300]]

main = do
    putStrLn $ show $ maxPowerAllSquares
