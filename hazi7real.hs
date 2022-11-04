module Hazi7 where

sum' :: (Int,Int) -> Int
sum' (a,b) = a+b

sumOfAll :: [(Int,Int)] -> Int
sumOfAll [] = 0
sumOfAll (x:xs) = sum' x + sumOfAll xs



absNegativeProduct :: (Num a, Ord a) => [a] -> a
absNegativeProduct [] = 1
absNegativeProduct (x:xs)
    | x < 0 = abs x * absNegativeProduct xs
    | otherwise = absNegativeProduct xs



doubleTriple :: Int -> Int
doubleTriple x
    | even x = x*2
    | odd x = x*3


addTwoThree :: Int -> Int
addTwoThree x
    | even x = x+2
    | odd x = x+3


applyFunction :: (Int -> Int) -> [Int] -> [Int]
applyFunction _ [] = []
applyFunction f (x:xs) = f x : applyFunction f xs



