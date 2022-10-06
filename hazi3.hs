module Hazi3 where

 putIntoList ::a -> [a]
 putIntoList a = [a]

 interval :: Int -> Int -> [Int]
 interval a b = [a..b]

 headTail :: [Int] -> (Int, [Int])
 headTail  a = (head a, tail a)
 
 doubleHead :: [Int] -> [Char] -> (Int, Char)
 doubleHead a b = (head a, head b)

 divFive :: [Int]
 divFive=[ x | x <- [4..144], even x,x `mod` 5 ==0]