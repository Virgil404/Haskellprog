module Zh1 where 
--Zaccomer Virgil


--első kérdés
--Integer mert egy egész szám faktoriálisa egész szám lesz és nincs limitje mint az int-nek 

--második kérdés
--Az Int tipus  csak egész számokat tud  megjeleníteni ezért a / operátort nem lehet rá használni 

task1 :: Bool -> Bool -> Bool -> Bool
task1 True True True =False
task1 True False True = False
task1 False False False = False
task1 False True False = True
task1 False True True = False
task1 False False True = True

tortMultiply :: (Int, Int) -> (Int, Int) -> (Int, Int)
tortMultiply (a,b) (c,d )= (a*c,b*d)

listTask :: [Int]
listTask = [ x | x <- [0..250], even x,x `mod` 5 ==3,x `mod` 3 ==0]