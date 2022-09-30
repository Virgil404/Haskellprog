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