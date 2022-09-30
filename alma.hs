module Lesson1 where 

one :: Int
one = 1

two :: Int

two = 2*one

inc :: Int -> Int
inc x =x+1

sumn :: Int -> Int -> Int
sumn x y = x+y 

istring :: String 
istring = "Hello World"

ischar :: Char 
ischar = 'a'


divide :: Int -> Int -> Int
divide a  b = a `div` b  



iseven :: Int -> Bool

iseven a = a `mod` 2 ==0 


 terulet :: Int -> Int -> Int 
 terulet x y = x * y 