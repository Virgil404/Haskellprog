module Hazi7 where 

sumSpheres :: Double -> Double -> Int
sumSpheres r1 r2
    | v <= 0 = 0
    | v <= 100 = 1
    | v <= 250 = 2
    | v <= 500 = 3
    | v <= 750 = 4
    | otherwise = 5
    where v = 4 / 3 * pi * (r1 ^ 3 + r2 ^ 3)


isSquareNum :: Int -> Bool
isSquareNum n = n >= 0 && (floor sqrtN) ^ 2 == n
    where sqrtN = sqrt (fromIntegral n)


listNotMatching :: [Int] -> [Int] -> [Int]
listNotMatching xs ys = [x | (i, x) <- zip [0..] xs, i >= length ys || x /= ys !! i]
