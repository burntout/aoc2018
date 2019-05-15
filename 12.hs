import Data.List

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

processPlants growFunc (offset, plants) = trim $ (offset + 3, nextGenPlants growFunc $ "....." ++ plants ++ ".....")

trim (offset, plants) = (offset - diff, drop diff newPlants)
    where
        diff = length $ takeWhile (== '.') plants
        newPlants = reverse $ dropWhile (== '.') $ reverse plants

result (offset, plants) = sum $ map (\x -> x - offset) $ elemIndices '#' plants

main = print $ result $ last $ take 21 $ iterate (processPlants myF) (0, inputData)

