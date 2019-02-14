import Data.Char
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    pointsText <- fmap Text.lines (Text.readFile "6.txt")
    let pointsString =  head $ map Text.unpack pointsText
    putStrLn $ show $ length pointsText 

data Point = Point { xcoord :: Int, ycoord :: Int } deriving (Eq, Show)

listToPoint [a,b] = Point { xcoord = a, ycoord = b}

distance :: Point -> Point -> Int
distance p1 p2 = xdiff + ydiff
    where
        xdiff = abs $ (xcoord  p1) - (xcoord p2)
        ydiff = abs $ (ycoord  p1) - (ycoord p2)
