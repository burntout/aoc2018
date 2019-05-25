import qualified Data.Sequence as S
import Data.Foldable

-- nextRecipeScores sums two digits, and outputs a 
-- list of the digits of the results
-- throwing away leading zero's
nextRecipeScores :: Int -> Int -> [Int]
nextRecipeScores a b
    | x == 0    = [y]
    | otherwise = [x, y]
    where 
        (x,y) = (a + b) `divMod` 10

-- nextRecipes lazily generates an infinite list of the next recipes
-- from a sequence of all current known recipes and the current elf indexes
-- It output the next elements of the recipe list, and append it to a sequence.
-- Update the elf indexes, and call it all again.

-- Spent a long time trying to make this pattern work

--        gen f = xs where xs = map f $ inits xs
-- 
-- It lazily generate a list where the next element depends on some function 
-- of all the preceding elements. 
-- but I couldn't see how to make it fit.
 
nextRecipes :: S.Seq Int -> (Int, Int) -> [Int]
nextRecipes rs ei = nr ++ (nextRecipes (rs <> (S.fromList nr)) (ne0i, ne1i))
    where 
        (e0i, e1i) = ei
        sc0 = S.index rs e0i
        sc1 = S.index rs e1i
        nr  = nextRecipeScores sc0 sc1
        newLength = (S.length rs) + (length nr)
        ne0i = (e0i + 1 + (S.index rs e0i)) `mod` newLength
        ne1i = (e1i + 1 + (S.index rs e1i)) `mod` newLength

recipes :: [Int]
recipes = 3 : 7 : nextRecipes (S.fromList [3,7]) (0,1)

-- find the index of sublist 'as' in list 'xxs'
match :: (Eq a, Num t) => [a] -> [a] -> t
match as xs = match' 0 as xs
match' n as xxs@(x:xs)
    | take (length as) xxs == as = n
    | otherwise                  = match' (n+1) as xs


-- Note this is still quite slow on my laptop
solutionPart1 = drop 286051 $ take 286061 recipes
solutionPart2 = match [2,8,6,0,5,1] recipes
main = putStrLn $ show $ (solutionPart1, solutionPart2)
