{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Control.Monad.State

-- Rather than parse the input file, I just dropped it into the file and edited it with vim
-- to create a function (myF)
-- It's easy to do do this in Haskell

inputData = "###..#...####.#..###.....####.######.....##.#####.##.##..###....#....##...##...##.#..###..#.#...#..#"

myF :: [Char] -> Char
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


-- This is just the test Data 
-- and test growth function

testData = "#..#.#..##......###...###"

testF :: [Char] -> Char
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

-- Given a grow function and a string generate a the next string
--
-- nextGenPlants takes elements 5 at a time from a list 
-- and applies the grow function to them
-- it doesn't properly deal with the first 2 or last 4 elements of the plant string
-- but don't care here.  We fix that later on by prefixing and suffixing 5 "."'s 
-- i.e. empty plant pots when we come use this in "processPlants"
--
nextGenPlants :: (String -> Char) -> String -> String
nextGenPlants growFunc xs
    | length xs > 4 = front ++ back
    | otherwise = []
    where
        front = [growFunc $ take 5 xs]
        back  = nextGenPlants growFunc $ tail xs

-- trim removes any leading or trailing '.' from the string representing the plants and pots
-- it also recalculates the offset (  that's how far to the left or right the initial element of the plant string is)
trim :: (Int, [Char]) -> (Int, [Char])
trim (offset, plants) = (offset - diff, drop diff newPlants)
    where
        diff = length $ takeWhile (== '.') plants
        newPlants = reverse $ dropWhile (== '.') $ reverse plants

-- processPlants takes a grow function, a tuple of an offset and a string representing the plants,
-- and does returns next generation
processPlants :: (String -> Char) -> (Int, String) -> (Int, String)
processPlants growFunc (offset, plants) = trim $ (offset + 3, nextGenPlants growFunc $ "....." ++ plants ++ ".....")

-- result takes a tuple of offset and plant string, finds the indexes of all the plants
-- then applies the offset to them so we know their actual position
-- finally we sum these to get the result
result :: (Int, String) -> Int
result (offset, plants) = sum $ map (\x -> x - offset) $ elemIndices '#' plants


solutionPart1 = result $ last $ take 21 $ iterate (processPlants myF) (0, inputData)

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

solutionPart2 = result ((50000000000 - generations) * offsetDiff + offset, plants)
    where 
        (generations, offsetDiff, (offset, plants)) = evalState (findStablePattern myF)  (0, (0, inputData))

main = do
    print solutionPart1 
    print solutionPart2

