import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    tls <- fmap Text.lines (Text.readFile "1.txt")
    let ss = map Text.unpack tls
        ints = map stringToInt ss
    putStrLn $ show $ sum ints

stringToInt s 
    | head s == '+' = stringToInt $ tail s
    | otherwise       = read s::Int


