module Lesson3 where

--Mintaillesztes

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False


first :: (a,b,c)->a 
first (x,_,_) = x 

listaint :: [Int]
listaint = [1,2,3,4,5]

listastring :: [String]
listastring = ["ab","cd","ef", "gh","ii"]


listabool :: [Bool]
listabool = [True ,False,  True, False,True ]


--funkciok
--head
--length 
--last 
--reverse 
--take 
--drop
--sum
--product
-- `elem`
--zip 
-- !!
-- listak listaja 

listainlista :: [[Int]]
listainlista = [[1,2,2,2,4],[2,3,4,6,7]]

--Listagenerator 

egyhusz :: [Int]
egyhusz = [1..20]

kisk:: [Char]
kisk = ['a'..'k']

nagyk:: [Char]
nagyk = ['A'..'Z']

--20-ig paros szamok

evento20 :: [Int]
evento20 = [x*2|x<-[1..10]]


evento20b :: [Int]
evento20b = [x|x<-[1..20],even x]

--evenfrom12::[Int]
--evenfrom12 [x| x<-[1..20],even x ,x>=12]

divfive::[Int]
divfive = [ x | x <- [1..100], x `mod` 7 ==5]


diveleven::[Int]
diveleven = [ x | x <- [1..1000], x `mod` 11 ==0]


divsix::[Int]
divsix = [ x | x <- [16..144], x `mod` 6 ==0,x `mod` 4 /=0]

multiplyList :: [Int]
multiplyList = [x*y | x<-[1,2,3], y<-[3,4,5]]

removeuppercase ::  [Char] -> [Char] 
removeuppercase st = [c | c <- st, c `elem` ['a'..'z']]

mtx1 :: (Int,Int,Int)
mtx1 = (1,2,4)

mtx2 :: (Int,Int,Int)
mtx2 = (4,6,9)

multiplytuple :: (Int,Int,Int) ->(Int,Int,Int) -> (Int,Int,Int)
multiplytuple (x,y,z) (x1,y1,z1) = (x*x1,y*y1,z*z1)