module Lesson7 where

doubleMe :: Int -> Int 
doubleMe x = 2*x 

doubleEvenList :: [Int]->[Int]
doubleEvenList = map doubleMe

nullEven :: Int -> Int 
nullEven x 
 | even x =0
 |otherwise =x 

nullList :: [Int]-> [Int]
nullList = map nullEven

hasFever :: Int -> Bool 
hasFever x = (fromIntegral (x-32)/1.8)>38

