import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    tls <- fmap Text.lines (Text.readFile "2.txt")
    let ss = map Text.unpack tls
        twos = sum $ map (number 2) ss
        threes = sum $ map (number 3) ss
    putStrLn $ show $ twos * threes

count []     = []
count (a:as) =(a, length (filter (==a) (a:as))) : (count $ filter (/= a) as)

number n str
    | (length $ filter (\x -> n == snd x ) $ count str) > 0 = 1
    | otherwise                                             = 0


