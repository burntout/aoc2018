import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map     as Map
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

data Node = Node { nodeName :: Char,
                   parents :: String, 
                   children :: String,
                   weight   :: Int
                 } deriving (Eq, Ord, Show)

main = do
    nodesText <- fmap Text.lines (Text.readFile "7.txt")
    let nodesStrings = map Text.unpack nodesText
        nodes = getNodes $ map stringToTuple nodesStrings 
        path = takeWhile (/= []) $ iterate f nodes
        solution = map nodeName $ map getNextToRemove  $ path
        numberCanRemove = map ((map nodeName) . canRemove) path
    putStrLn solution 
    print $ show numberCanRemove

stringToTuple s = (a, b)
    where 
        w = words s 
        a = head $ w!!1
        b = head $ w!!7
        
testData = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]

findAllChildren a as = (map snd $ filter (\x -> a == fst x) as)
findAllParents a as = (map fst $ filter (\x -> a == snd x) as)
allID as = nub $ (map fst as) ++ (map snd as)

getNodes xs = [ Node {nodeName = x, parents = findAllParents x xs, children = findAllChildren x xs, weight = charToWeight x} | x <- allID xs ]

charToWeight :: Char -> Int
charToWeight c = ord c - 4 


canRemove xs = filter (\x -> parents x == "") $ sort xs
getNextToRemove xs = head $ canRemove xs

removeNode x xs = ys
    where
        bs = filter (/= x) xs
        ys = [Node {nodeName = nodeName y, parents = filter (/=(nodeName x)) $ parents y, children = children y, weight = weight y}  | y <- bs] 

f x = removeNode (getNextToRemove x) x

queues :: [[Node]]
queues = [[],[]]

subWeight x y = max 0 $ x - y  

sortNodeByWeight n1 n2 
    | weight n1 < weight n2 = LT
    | weight n1 > weight n2 = GT
    | weight n1 == weight n2  = compare (nodeName n1) (nodeName n2)
    

