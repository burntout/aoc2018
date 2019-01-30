import Data.List
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    tls <- fmap Text.lines (Text.readFile "2.txt")
    let ss = map Text.unpack tls
        strPairs = pairs ss 
        result = head $ filter (/= []) $ map oneDifferences strPairs
        first = head result
        second = last result
    putStrLn $ show $ commonChar first second
   
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

numDifferences (xs, ys) = length $ filter ( == True) $ zipWith (\x y -> x /= y) xs ys

oneDifferences (xs, ys) 
    | numDifferences (xs, ys)  == 1  = [xs,ys]
    | otherwise                      = []

commonChar [] [] = []
commonChar (x:xs) (y:ys)
    | x == y    = x : commonChar xs ys
    | otherwise = commonChar xs ys
