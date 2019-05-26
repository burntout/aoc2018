import Control.Monad.State
import qualified Data.Sequence as S

-- nextRecipeScores sums two digits, and outputs a 
-- list of the digits of the results
-- throwing away leading zero's
nextRecipeScores :: Int -> Int -> [Int]
nextRecipeScores a b
    | x == 0    = [y]
    | otherwise = [x, y]
    where 
        (x,y) = (a + b) `divMod` 10

-- nextRecipeState takes a sequence of recipes and an elfIndex and 
-- and outputs an updated sequence of recipes and elf indexes
 
nextRecipeState :: (S.Seq Int, (Int, Int)) -> (S.Seq Int, (Int, Int))
nextRecipeState (rs, ei) = (rs <> (S.fromList nr), (ne0i, ne1i))
    where 
        (e0i, e1i) = ei
        sc0 = S.index rs e0i
        sc1 = S.index rs e1i
        nr  = nextRecipeScores sc0 sc1
        newLength = (S.length rs) + (length nr)
        ne0i = (e0i + 1 + (S.index rs e0i)) `mod` newLength
        ne1i = (e1i + 1 + (S.index rs e1i)) `mod` newLength


recipeLoop = do 
    (currentRecipes, currentElfIndex) <-  get
    let (nextRecipes, nextElfIndex) = nextRecipeState (currentRecipes, currentElfIndex)
    put (nextRecipes, nextElfIndex)
    if not $ stopCondition nextRecipes
        then recipeLoop
        else return nextRecipes

final = S.fromList [2,8,6,0,5,1] 
-- final = S.fromList [5,9,4,1,4] 
stopCondition recipes = test1 || test2
    where 
        n = length final
        test1 = (S.reverse $ S.take n $ S.reverse recipes) == final
        test2 = (S.reverse $ S.drop 1 $ S.take (n+1) $ S.reverse recipes) == final

doRecipeLoop = evalState recipeLoop (S.fromList [3,7], (0, 1))

solutionPart2 = length doRecipeLoop - (length final)

main = print $ solutionPart2
