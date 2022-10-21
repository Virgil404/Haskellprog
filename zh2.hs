{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ZH2 where 
--1
-- egy függyvényt aminek több argumentuma van feloszt több kisebb függvényre pl 
-- A(2,3,4)
-- b(2)
-- c(3)
-- d(4)

--2 nincs lekezelve ha valamelyik tömb üres

tuplesMultiply :: [(Int,Int)] -> [Int]
tuplesMultiply xs = [add a v | (a,v) <- xs]
    where add a v = a*v


sumSphereCube :: Double -> Double -> Int
sumSphereCube r a
    | x == 0 = 0
    | x >= 1 && x <= 100 = 1
    | x >= 101 && x <= 250 = 2
    | x >= 251 && x <= 500 = 3
    | x >= 501 && x <= 750 = 4
    | otherwise = 5
    where x = (4*(r^3)*pi)/3 + (a^3)

listNotMatching :: (Eq a) => [a] -> [a] -> [a]
listNotMatching _ [] = []
listNotMatching [] _ = []
listNotMatching (x:xs) (y:ys)
    | x /= y = x : listNotMatching xs ys
    | otherwise = listNotMatching xs ys