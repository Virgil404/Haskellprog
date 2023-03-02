module Lessonrep1 where

doub :: Double
doub =  4.3/2.4

floa:: Float 
floa = 4.3/2.4



inc :: Int -> Int 
inc a = a + 1

inc' :: Int -> Int 
inc' a = (+) a 1 


letterE :: Char 
letterE = 'E'


word :: [Char]
word ="word"

string1 :: String
string1 = "String1"


isEven :: Int-> Bool
isEven a = a `mod`2 ==0


isODD :: Int-> Bool
isODD a = a `mod`2 /=0


sum' :: Int-> Int-> Int 
sum' a b = a+b 


voluem :: Num a => a -> a 
voluem b = b *b *b 

isODD2 :: Int -> Bool
isODD2 a = not(isEven a )



