module Orerep2 where 

--Tuple 
tuple1::(Int,Int)
tuple1 = (1,2)

tuple3 :: (Int, Int )
tuple3= (1,2)


tuple2 :: (Int, Int , Int)
tuple2= (1,2,3)

dateTuple :: (Int, String , Int)
dateTuple = (9, "Marcius",2023)

dateTuple2 :: (Int, Int , Int)
dateTuple2 = (9, 3,2023)

mixedTuple :: (Int, String, Bool, Float)
mixedTuple = (5,"sajt",True, 2.4)

getYear :: (Int, Int, Int)->Int
getYear (nap,honap,ev)= ev 

addTupe :: (Int, Int)->Int
addTupe (x,y)= x+y

addTuples :: (Int, Int)-> (Int,Int)-> (Int, Int)
addTuples (x1,y1) (x2,y2)= (x1+x2,y1+y2)

swaptuple :: (Int, Int )-> (Int, Int )
swaptuple (x,y)=(y,x)

isEventuple :: Int ->(Int, Bool)
isEventuple a = (a, even a)

--domino

dominomathcing :: (Int, Int) -> (Int, Int)->Bool 
dominomathcing (x,y)(x2,y2)= (x==x2) || (y==y2) ||(x==y2) || (y2==x2)

isSame ::(Int, Int) -> (Int, Int)->Bool 
isSame (x,y)(x2,y2) = ((x==x2)||(x==y2))&&((y==x2)||(y==y2))


dontMatch :: (Int, Int) -> (Int, Int)->Bool 
dontMatch(x,y)(x2,y2)=((x/=x2)||(x/=y2))&&((y/=x2)||(y/=y2))

--Típusosztályok


