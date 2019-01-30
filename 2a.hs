import Data.List
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    tls <- fmap Text.lines (Text.readFile "2.txt")
    let ss = map Text.unpack tls
        strPairs = pairs ss 
        boxIDs = fst $ head $ filter (\x -> snd x == 1) strPairs
        box1 = fst boxIDs
        box2 = snd boxIDs
    putStrLn $ show $ commonChar box1 box2
   
pairs l = [((x,y), numDifferences x y) | (x:ys) <- tails l, y <- ys]

numDifferences xs ys = length $ filter (== True) $ zipWith (\x y -> x /= y) xs ys

commonChar [] [] = []
commonChar (x:xs) (y:ys)
    | x == y    = x : commonChar xs ys
    | otherwise = commonChar xs ys
