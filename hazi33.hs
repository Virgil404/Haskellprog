module Hazi3 where 

    sumTo100 :: Int
    sumTo100 = sum [1..100]

    divBy4 :: Int -> Bool
    divBy4 n = n `mod` 4 == 0

    squareNums :: [Int]
    squareNums = take 10 [2^n | n <- [1..]]


