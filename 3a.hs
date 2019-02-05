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
    putStrLn  $ show $ fno claims

stringToClaim str = Claim { claimID = claimID, corner = (x,y), xLength = xl, yLength =  yl }
    where 
        ws = words str
        claimID = read $ tail $ ws!!0  :: Int
        x = read ((splitOn "," $ init $  ws!!2)!!0) :: Int
        y = read ((splitOn "," $ init $  ws!!2)!!1) :: Int
        xl = read ((splitOn "x" $ ws!!3)!!0) :: Int
        yl = read ((splitOn "x" $ ws!!3)!!1) :: Int

corners c = [(x0,y0),( x1,y0), (x0,y1), (x1,y1)]
    where 
        (x0,y0) = corner c
        (x1,y1) = (x0 + xLength c - 1, y0 + yLength c - 1)

hasoverlaps a b  = (x0 < x1) && (y0 < y1) 
    where 
        x0 = maximum $ map (fst . xcoords) [a, b]
        x1 = minimum $ map (snd . xcoords) [a, b]
        y0 = maximum $ map (fst . ycoords) [a, b]
        y1 = minimum $ map (snd . ycoords) [a, b]
        
xcoords c = (cx0, cx1)
    where 
        cx0 = fst $  corner c
        cx1 = cx0 + xLength c 

ycoords c = (cy0, cy1)
    where 
        cy0 = snd $  corner c
        cy1 = cy0 + yLength c 
            

overlaps c cs = filter (hasoverlaps c) cs

fno cs = [c | c <- cs, (length $ overlaps c cs) == 1]
