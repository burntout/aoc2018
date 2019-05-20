import qualified Data.Sequence as S

initElfIndexes:: (Int, Int)
initElfIndexes = (0,1)
initRecipes:: S.Seq Int
initRecipes = S.fromList [3,7]
nextRecipeScores :: Int -> Int -> S.Seq Int
nextRecipeScores a b = x S.>< y
    where 
        y = S.fromList [(a + b) `mod` 10]
        x = first a b 
        first a b 
            | (a + b) `div` 10  == 1  = S.fromList [1]
            | otherwise               = S.fromList []

nextRecipes rs ei = rs S.>< nr
    where 
        sc0 = S.index rs (fst ei)
        sc1 = S.index rs (snd ei)
        nr  = nextRecipeScores sc0 sc1

nextElfIndexes rs ei = (ne0i, ne1i)
    where 
        e0i = fst ei
        e1i = snd ei
        newLength = S.length $ nextRecipes rs ei
        ne0i = (e0i + 1 + S.index rs (fst ei)) `mod` newLength
        ne1i = (e1i + 1 + S.index rs (snd ei)) `mod` newLength
-- 
genRecipes (rs, ei) = (nextRecipes rs ei, nextElfIndexes rs ei)

slowSolutionPart1 = S.take 10 $ S.drop 286051 $ last $ Prelude.take 286072 $ map fst $ iterate genRecipes (initRecipes, initElfIndexes)

main = print slowSolutionPart1
