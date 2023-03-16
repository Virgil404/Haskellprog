module Orarep3 where 

    mtx1::(Int,Int,Int)
    mtx1 =(1,3,4)

    
    mtx2::(Int,Int,Int)
    mtx2 =(2,5,4)

    mtxszorzas :: (Int,Int,Int)->(Int,Int,Int)->Int
    mtxszorzas (x,y,z) (x2,y2,z2)=x*x2+y*y2+z*z2


    lista1 :: [Int]
    lista1 = [1,2,3,4,5]

    lista2 :: [String]
    lista2 = ["majom","majom"]

    lista3 :: [Char]
    lista3 = ['a','f','h']

    listinList:: [[Int]]
    listinList = [[1,2,3,4],[2,4,6,8],[2,7,6,0]]

    head' :: [a]->a 
    head' (x:xs)=x

    tail' :: [a]->[a]
    tail' (x:xs)=xs