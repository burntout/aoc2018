-- This is the naive solution for Q11.. 
-- It's OK for the 1st part, however very inefficient for the 2nd

power p = ((((x + 10)* y) + sn) * (x + 10) `div` 100 `mod` 10) -5 
    where
        x = fst p
        y = snd p
        sn = 2694

powerArea p z = sum $ map power [(x, y) |  x <- [x .. x + z - 1 ], y <- [y .. y + z - 1]]
    where
        x = fst p
        y = snd p

-- totals = [(powerArea (x,y) z, (z,x,y)) | z <- [1..300], x <-[1 .. 301-z], y <-[1 .. 301-z]]
totals = [(powerArea (x,y) z, (x,y,z)) | z <- [3], x <-[1 .. 301-z], y <-[1 .. 301-z]]

main = putStrLn $ show $ maximum totals
-- main = putStrLn $ show $ totals
