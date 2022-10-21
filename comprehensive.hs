--Comprehensive

--module Hazi1 where 

--string -konstans
firstName :: String 
firstName = "Nevem"

--double -kifejezes
doubleExpr :: Double
doubleExpr = 6/9

--bool -kifejezes
boolExpr :: Bool
boolExpr = 0 < 1

--harom szam szorzata -fuggveny
multiplyThree :: Int -> Int -> Int ->Int
multiplyThree a b c  = a*b*c

--module Hazi2 where 

--5 es 15 kozti prim szam e -fuggveny
isSmallPrime:: Int -> Bool
isSmallPrime a = a == 5 || a ==11 || a == 7 || a == 13

--egyenlo e ket logikai valtozo -fuggveny
equivalent:: Bool -> Bool -> Bool
equivalent a b = a==b

--implikalja e egyik (logikai) a masikat -fuggveny?
implies:: Bool->Bool->Bool
implies a b = if a==True&&b==False 
      then False
   else True

--ket pont tavolsaga x tengelyen -fuggveny
xDistance::(Int, Int) -> (Int, Int) -> Int
xDistance (a1,a2) (b1,b2) = abs(a1-b1)

--rajta van e y=-2x egyenesen -fuggveny
isOnNeg2X :: (Int, Int) -> Bool
isOnNeg2X (a,b) =  b == (-2 * a)

--x tengelyre tukrozo -fuggveny
invertX :: (Int, Int) -> (Int, Int)
invertX (a,b) = (a,-b)

--module Hazi3 where

--egy elemu listaba rakja -fuggveny
 putIntoList ::a -> [a]
 putIntoList a = [a]

--ket szam kozti intervallum -fuggveny
 interval :: Int -> Int -> [Int]
 interval a b = [a..b]

--lista feejbe es farokba rak -fuggveny
 headTail :: [Int] -> (Int, [Int])
 headTail  a = (head a, tail a)
 
 --ket lista elso elemei rendezett parba -fuggveny
 doubleHead :: [Int] -> [Char] -> (Int, Char)
 doubleHead a b = (head a, head b)

--4..144 kozti 5el es 2vel oszthato szamok -fuggveny
 divFive :: [Int]
 divFive=[ a | a <- [4..144], even a,a `mod` 5 ==0]
 
--module Hazi4 where 

--adott listabol 10..100 kozti 3al es 2vel oszthato -fuggveny
task1 :: [Int] -> [Int]
task1 a= [x|x<-a, x `mod`3==0,even x, x>10 ,x<100]

--y es Y talanito -fuggveny
task2 :: String -> String
task2 a=[x|x<-a,x/='y',x/='Y']

--lista elemeit haromszorozo -fuggveny (ures->hiba)
task3 :: [Int] -> [Int]
task3 [] = error "ures"
task3 a = [3*x|x<-a]

--2 lista elemeinek osszeszorzasa uj listaba -fuggveny
task4 :: [Int] -> [Int] -> [Int]
task4 a b = [x*y| x<-a ,y<-b, x/=0,y/=0]

--module Hazi5 where

--onmagat meghivo (rekurziv) faktorialis -fuggveny
factorial :: Integer -> Integer
factorial n | n < 0 = error "no"
      | n == 0 = 1
      | n == 1 = 1
      | n > 1 = n * factorial (n-1)

--nagysag szerint kategorizalo -fuggveny
fogyasztas :: Double -> Double -> Int
fogyasztas a b
 | c <=0.05 =1
 | c <=0.075 =2
 | c <=0.10 =3
 |otherwise = 4
 where c = b/a

--pontosan hossz (itt 1) igazsagerteke -fuggveny
isSingleton :: [a] -> Bool
isSingleton a
    | length a == 1 = True
    | otherwise = False

--bekert szam minden osztojat listaba -fuggveny
divisors :: Int -> [Int]
divisors a = [ x | x <- [1..a], a `mod` x ==0]