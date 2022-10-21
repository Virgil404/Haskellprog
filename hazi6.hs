{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Hazi5 where




factorial :: Integer -> Integer
factorial n | n < 0 = error "nah"
      | n == 0 = 1
      | n == 1 = 1
      | n > 1 = n * factorial (n-1)

fogyasztas :: Double -> Double -> Int
fogyasztas km liter
 | fogyaszt <=0.05 =1
 | fogyaszt <=0.075 =2
 | fogyaszt <=0.10 =3
 |otherwise = 4
 where fogyaszt = liter/km

isSingleton :: [a] -> Bool
isSingleton a
    | length a == 1 = True
    | otherwise = False

divisors :: Int -> [Int]
divisors a = [ x | x <- [1..a], a `mod` x ==0]
 