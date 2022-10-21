{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ora6 where 



--primes :: Int -> [Int]
--primes a = filterPrime [2..a]
-- where filterPrime (p:xs)= p: filterPrime[x| x<-xs,x`mod`p/=0 ]

bmitell :: Double -> Double -> String
bmitell weight height
 | bmi <=18.5 = "Underweight"
 | bmi <= 25.0 ="normal"
 | bmi <= 30.0="owerweight"
 |otherwise = "Big"
 where bmi = weight/height^2

 
bmitell2 :: Double -> Double -> String
bmitell2 weight height
 | bmi <=skinny = "Underweight"
 | bmi <= normal ="normal"
 | bmi <= overweight="owerweight"
 |otherwise = "Big"
 where bmi = weight/height^2
       (skinny, normal, overweight) = (18.5,25.0,30.0)


max' :: (Ord a )=> a -> a -> a 
max' a b 
   |a>b=a 
   |otherwise = b

   
min' :: (Ord a )=> a -> a -> a 
min' a b 
   |a<b=a 
   |otherwise = b

mycompare :: (Ord a)=> a -> a->Ordering
mycompare a b 
    | a>b = GT
    | a==b = EQ
    | otherwise = LT

reverse' :: [a]-> [a]
reverse' []=[]
reverse' (x:xs)= reverse' xs ++ [x]

replicate':: (Num i , Ord  i) => i -> a -> [a]
replicate' n x 
    | n <=0=[]
    | otherwise =x:replicate'(n-1) x

maximum' :: (Ord a )=> [a]-> a 
maximum' []= error "Empty list"
maximum' [x]= x 
maximum' (x:xs)
    | x > maxTail=x
    |otherwise = maxTail
    where maxTail = maximum' xs
    
minimum' :: (Ord a )=> [a]-> a 
minimum' []= error "Empty list"
minimum' [x]= x 
minimum' (x:xs)
    | x < minTail=x
    |otherwise = minTail
    where minTail = minimum' xs 


zip' :: [a]-> [b]-> [(a,b)]
zip' _ []= []
zip' [] _= []
zip' (x:xs) (y:ys)= (x,y): zip' xs ys

isin :: (Eq a)=> a -> [a]-> Bool
isin a [] = False
isin a (x:xs)
    | a == x = True
    | otherwise = isin a xs