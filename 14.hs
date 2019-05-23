import qualified Data.Sequence as S
import Data.Foldable

initElfIndexes:: (Int, Int)
initElfIndexes = (0,1)


initRecipes:: S.Seq Int
initRecipes = S.fromList [3,7]

nextRecipeScores :: Int -> Int -> S.Seq Int
nextRecipeScores a b
    | x == 0    = S.fromList [y]
    | otherwise = S.fromList [x, y]
    where 
        (x,y) = (a + b) `divMod` 10

-- Given a Sequence of recipes and a pair of Elf indexes, append the next  recipes to the Sequence
nextRecipes :: S.Seq Int -> (Int, Int) -> S.Seq Int
nextRecipes rs ei = rs S.>< nr
    where 
        (e0i, e1i) = ei
        sc0 = S.index rs e0i
        sc1 = S.index rs e1i
        nr  = nextRecipeScores sc0 sc1

-- Given a Sequence of recipes and a pair of Elf indexes, generate the next Elf Indexes
nextElfIndexes :: S.Seq Int -> (Int, Int) -> (Int, Int)
nextElfIndexes rs ei = (ne0i, ne1i)
    where 
        (e0i, e1i) = ei
        newLength = S.length $ nextRecipes rs ei
        ne0i = (e0i + 1 + S.index rs e0i) `mod` newLength
        ne1i = (e1i + 1 + S.index rs e1i) `mod` newLength

-- Given the game state (i.e all current recipes and the current Elf indexes), generate the next game state
genRecipes :: (S.Seq Int, (Int, Int)) -> (S.Seq Int, (Int, Int))
genRecipes (rs, ei) = (nextRecipes rs ei, nextElfIndexes rs ei)


-- iterate genRecipes over the initial state
-- map fst over this, discarding the history of the elf indexes
-- take enough of these to make sure that we've gone past the place we needed to
-- take the last of these sequences.  because each iteration has all recipes up that place
-- drop the ones we don't want
-- take the ones we need.
slowSolutionPart1 :: S.Seq Int
slowSolutionPart1 = S.take 10 $ S.drop 286051 $ last $ take 286072 $ map fst $ iterate genRecipes (initRecipes, initElfIndexes)


-- a little function to take a list of sequences, and take them two at a time,
-- returning a new sequence containing just the ones added to the end of the second that aren't from the first
f :: [S.Seq a] -> [S.Seq a]
f (x:y:xs) = S.drop (S.length x) y : f (y:xs)

-- generate an infinite list of the recipes
-- iterate genRecipes over the initial state
-- map fst over this, discarding the history of the elf indexes
-- since each sequence contains the full recipe history, from each sequence, throw away the sequence elements that we already have
-- turn the sequences into a list
-- concatenate them all into a single list
-- and then prepend the starting position back
recipes :: [Int]
recipes = (++) [3,7] $ concat $ map toList $ f $ map fst $ iterate genRecipes (initRecipes, initElfIndexes)

-- find the index of sublist 'as' in list 'xxs'
match :: (Eq a, Num t) => [a] -> [a] -> t
match as xxs = match' 0 as xxs
    where
        match' n as xxs@(x:xs)
            | take (length as) xxs == as = n
            | otherwise                  = match' (n+1) as xs
        
slowSolutionPart2 :: Integer
slowSolutionPart2 = match [2,8,6,0,5,1] recipes

main = print slowSolutionPart2
