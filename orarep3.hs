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

    --Listagenerátorok

    toTwenty :: [Int]
    toTwenty = [1..20]

    toK :: [Char]
    toK = ['a'..'y']

    toTwentyEven :: [Int]
    toTwentyEven = [ x| x<-[1..20], even x ]

    toTwentyEven2 :: [Int]
    toTwentyEven2 = [2,4..20]

    toTwentyEven3 :: [Int]
    toTwentyEven3 = [ x*2| x<-[1..10] ]

    toTwentyEven4 :: [Int]
    toTwentyEven4 = [ x| x<-[12..20], even x ]

    to100mod7 :: [Int]
    to100mod7 = [ x| x<-[1..100],  x `mod`7==5 ]

    divby11 :: [Int]
    divby11 = [ x| x<-[1..1000],  x `mod`11==0 ]

    divby6not4 :: [Int]
    divby6not4 = [ x| x<-[16..144],  x `mod`6==0,x `mod`4/=0 ]

    multiplelist :: [Int]
    multiplelist = [x*y| x<- [1..10], y<-[2..12]]

    removeuppercase ::[Char]->[Char]
    removeuppercase st = [x | x<-st, notElem x ['A'..'Z']]

    someLetter :: [Char]
    someLetter = [x | x<-['A'..'k'], elem x ['A'..'K'] || elem x ['Q'..'Z']]


   