import qualified Data.Set     as Set
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    tls <- fmap Text.lines (Text.readFile "1.txt")
    let ss = map Text.unpack tls
        ints = map stringToInt ss
        foreverInts = cycle ints
        partialSums = scanl (+) 0 foreverInts
    putStrLn $ show $ getFirstDuplicate partialSums Set.empty

stringToInt s 
    | head s == '+' = stringToInt $ tail s
    | otherwise       = read s::Int

getFirstDuplicate (x:xs) s
    | Set.member x s = x
    | otherwise      = getFirstDuplicate xs $ Set.insert x s

