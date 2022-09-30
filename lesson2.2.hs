module Lesson2_2 where 

getYear :: (Int, Int, Int ) -> Int
getYear (x,y,z)=z

addtuple :: (Int, Int)->Int
addtuple (x,y) = x+y

addtuples :: (Int, Int) -> (Int,Int)->(Int, Int)
addtuples (x1,y1) (x2,y2) =(x1+x2,y1+y2)


isEvenTuple :: Int -> (Int,Bool)
isEvenTuple x = (x, even(x))

dominomatch :: (Int,Int) -> (Int,Int)->Bool
dominomatch (x1,y1) (x2,y2) = (x1==x2) || (x1==y2) || (y1==y2)||(y1==x2)

wheretest :: Int-> Int -> Int 
wheretest x y =b+y
 where b = 3*x+5
 
logical
