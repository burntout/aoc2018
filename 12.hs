{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Control.Monad.State

inputData = "###..#...####.#..###.....####.######.....##.#####.##.##..###....#....##...##...##.#..###..#.#...#..#"

myF ".###." = '.'
myF "..#.." = '.'
myF ".####" = '.'
myF ".##.." = '#'
myF "#.#.#" = '.'
myF "..#.#" = '#'
myF "#.##." = '#'
myF "#...#" = '#'
myF "....." = '.'
myF "##..#" = '#'
myF ".#.#." = '.'
myF "..##." = '#'
myF "##.#." = '.'
myF "###.." = '.'
myF ".#..." = '#'
myF "..###" = '.'
myF "#..##" = '.'
myF "...#." = '.'
myF "###.#" = '#'
myF ".##.#" = '.'
myF ".#.##" = '.'
myF "....#" = '.'
myF "#####" = '.'
myF "#.#.." = '#'
myF "...##" = '#'
myF "#...." = '.'
myF "#.###" = '#'
myF "##..." = '#'
myF ".#..#" = '.'
myF "####." = '.'
myF "#..#." = '#'
myF "##.##" = '#'

testData = "#..#.#..##......###...###"

testF "...##" = '#'
testF "..#.." = '#'
testF ".#..." = '#'
testF ".#.#." = '#'
testF ".#.##" = '#'
testF ".##.." = '#'
testF ".####" = '#'
testF "#.#.#" = '#'
testF "#.###" = '#'
testF "##.#." = '#'
testF "##.##" = '#'
testF "###.." = '#'
testF "###.#" = '#'
testF "####." = '#'
testF _       = '.'

nextGenPlants growFunc xs
    | length xs > 4 = front ++ back
    | otherwise = []
    where
        front = [growFunc $ take 5 xs]
        back  = nextGenPlants growFunc $ tail xs

trim (offset, plants) = (offset - diff, drop diff newPlants)
    where
        diff = length $ takeWhile (== '.') plants
        newPlants = reverse $ dropWhile (== '.') $ reverse plants

processPlants growFunc (offset, plants) = trim $ (offset + 3, nextGenPlants growFunc $ "....." ++ plants ++ ".....")

result (offset, plants) = sum $ map (\x -> x - offset) $ elemIndices '#' plants

-- loop to find a stable plant pattern
-- the pattern stays the same, but the offset alters
-- returns (numGenerations, offsetDiff, (offset, plantPattern))
findStablePattern growFunc = do
    (genCnt, (offset, plants)) <- get
    let (newOffset, newPlants) = processPlants growFunc (offset, plants)
    put (genCnt + 1, (newOffset, newPlants))
    if (plants == newPlants)
        then return (genCnt + 1, newOffset - offset, (newOffset, newPlants))
        else findStablePattern growFunc

(generations, offsetDiff, (offset, plants)) = evalState (findStablePattern myF)  (0, (0, inputData))

-- print part 2 solution
main = print $ result ((50000000000 - generations) * offsetDiff + offset, plants)

-- Solution to Part 1 
-- main = print $ result $ last $ take 21 $ iterate (processPlants myF) (0, inputData)
