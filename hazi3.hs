module Hazi3 where

 putIntoList :: Int -> [Int]
 putIntoList a = [a]

 interval :: Int -> Int -> [Int]
 interval a b = [a..b]

 headTail :: [Int] -> (Int, [Int])
 headTail  a = (head a, tail a)
 
 doubleHead :: [Int] -> [Int] -> (Int, Int)
 doubleHead a b = (head a, head b)
 
 divFive :: [Int]
 divFive=[ x | x <- [16..144], even x,x `mod` 5 ==0]