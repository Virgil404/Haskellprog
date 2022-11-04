module Hazisok where 

    doubleDivBy3 :: Int -> Int
    doubleDivBy3 a 
     | a `mod` 3==0 = a*2
     | otherwise =a


    doubleDivBy3Map :: [Int] -> [Int ]
    doubleDivBy3Map = map doubleDivBy3

    isOverheating :: Double -> Bool
    isOverheating a = celsius > 95
     where celsius = a -273.15

    isOverheatingList :: [Double] -> [Bool]
    isOverheatingList = map isOverheating




    --alidGame :: String ->Bool 
    --validGame xs = check (words xs) where 
      --  check :: [String]->Bool
        --check (x:y:xs)
      -- |last x == head y = True
        -- | otherwise False
       --check _=True


    addThree:: Num a => a -> a 
    addThree x = x+3 


    kivonas :: Num a => a->a -> a 
    kivonas  x y =  x - y


    applyTwice :: (a -> a )-> a -> a
    applyTwice f x = f(f x) 



    map' :: (a->b)->[a]->[b]
    map' _ []=[]
    map' f (x:xs) = f x : map' f xs

    map2 :: (a->b)-> [a]->[b]
    map2 f li = [f x | x<-li]


    filter' :: (a -> Bool) -> [a]-> [a]
    filter' _ []=[]
    filter' p (x:xs)
        | p x = x : filter' p xs 
        | otherwise =filter' p xs