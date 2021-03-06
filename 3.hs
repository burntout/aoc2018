import Data.List
import Data.List.Split
import qualified Data.Set     as Set
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

data Claim = Claim  { claimID :: Int
                    , corner :: (Int, Int)
                    , xLength :: Int
                    , yLength :: Int 
                    } deriving (Show)

main = do
    claimText <- fmap Text.lines (Text.readFile "3.txt")
    let claimStrings =  map Text.unpack claimText
        claimPoints = map (claimToPoints . stringToClaim) claimStrings
        pairClaims = pairs claimPoints
        overlaps = Set.unions $ map (\x -> Set.intersection (fst x) (snd x)) pairClaims
        

    putStrLn $ show $ length $ overlaps
  

stringToClaim str = Claim { claimID = claimID, corner = (x,y), xLength = xl, yLength =  yl }
    where 
        ws = words str
        claimID = read $ tail $ ws!!0  :: Int
        x = read ((splitOn "," $ init $  ws!!2)!!0) :: Int
        y = read ((splitOn "," $ init $  ws!!2)!!1) :: Int
        xl = read ((splitOn "x" $ ws!!3)!!0) :: Int
        yl = read ((splitOn "x" $ ws!!3)!!1) :: Int

claimToPoints claim = Set.fromList [ (x,y) | x <- [x1 .. x2], y <- [y1 .. y2]]
    where
        (x1, y1) = corner claim
        x2 = x1 + xLength claim - 1
        y2 = y1 + yLength claim - 1

pairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys] 

