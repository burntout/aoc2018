import Data.List
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    tls <- fmap Text.lines (Text.readFile "2.txt")
    let sortedStrings = quickSort $ map Text.unpack tls

        differences = zipWith (\x y -> ((x,y), numDifferences x y)) sortedStrings $ tail sortedStrings
        boxes = fst $ head $ filter (\x -> snd x == 1) differences
        box1 = fst boxes
        box2 = snd boxes
    putStrLn $ show $ commonChar box1 box2
   
numDifferences xs ys = length $ filter (== True) $ zipWith (\x y -> x /= y) xs ys

commonChar [] [] = []
commonChar (x:xs) (y:ys)
    | x == y    = x : commonChar xs ys
    | otherwise = commonChar xs ys

quickSort []     = []
quickSort (x:xs) = quickSort small ++ (x:quickSort large)
    where
        small = [y | y<-xs, y<x] 
        large = [y | y<-xs, y>x] 


