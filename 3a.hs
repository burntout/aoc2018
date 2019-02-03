import Data.List
import Data.List.Split
import qualified Data.Set     as Set
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

data Claim = Claim  { claimID :: Int
                    , corner :: (Int, Int)
                    , xLength :: Int
                    , yLength :: Int 
                    } deriving (Eq, Show)

main = do
    claimText <- fmap Text.lines (Text.readFile "3.txt")
    let claimStrings =  map Text.unpack claimText
        claims = map stringToClaim claimStrings
    putStrLn $ show $ findNonOverlapping claims
    -- putStrLn $ show $ findNonOverlap claims
    --    c = getClaim 88 claims
    -- putStrLn  $ show $ fno claims

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

intersectClaims x y = Set.intersection xp yp
    where xp = claimToPoints x
          yp = claimToPoints y

findNonOverlapping cs = filter (\x -> snd x == Set.empty) [(c, Set.unions $ map (intersectClaims c) $ filter (\x -> c /= x) cs) |  c <-cs]

hasOverlap x y = Set.empty /= intersectClaims x y 

overlaps c cs = filter (hasOverlap c) cs

findNonOverlap (c:cs) 
    | overlaps c cs == [] = c
    | otherwise =  findNonOverlap $ cs \\ overlaps c cs

getClaim n cs = head $ filter (\x -> claimID x == n) cs

fno cs = [c | c <- cs, 1 == (length $ overlaps c cs)]
