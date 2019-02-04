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
    -- putStrLn $ show $ findNonOverlapping claims
    -- putStrLn $ show $ findNonOverlap claims
    --    c = getClaim 88 claims
    putStrLn  $ show $ fno1 claims

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

intersectClaims x y = Set.intersection xp yp
    where xp = claimToPoints x
          yp = claimToPoints y

hasOverlap x y = Set.empty /= intersectClaims x y 

overlaps c cs = filter (hasOverlap c) cs

corners c = [(x0,y0),( x1,y0), (x0,y1), (x1,y1)]
    where 
        (x0,y0) = corner c
        (x1,y1) = (x0 + xLength c - 1, y0 + yLength c - 1)


hasoverlaps1' a b  = and $ map (\x -> (inRange (ax0,ax1) (fst x) && (inRange (ay0, ay1) (snd x)))) $ corners b 
    where
        (ax0, ay0) =  corner a
        (ax1,ay1) = (ax0 + xLength a - 1,  ay0 + yLength a - 1)
        inRange (a,b) x  = (a <= x) && (x <= b)

hasoverlaps1 a b = (hasoverlaps1' a b) || (hasoverlaps1' b a) 

overlaps1 c cs = filter (hasoverlaps1 c) cs

fno1 cs = [c | c <- cs, (length $ overlaps1 c cs) == 1]



findNonOverlap (c:cs) 
    | overlaps c cs == [] = c
    | otherwise =  findNonOverlap $ cs \\ overlaps c cs

getClaim n cs = head $ filter (\x -> claimID x == n) cs

fno cs = [c | c <- cs, (length $ overlaps c cs) == 1]
