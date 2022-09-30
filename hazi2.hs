module Hazi2 where 

isSmallPrime:: Int -> Bool
isSmallPrime x = x == 5 || x ==11 || x == 7 || x == 13

equivalent:: Bool -> Bool -> Bool
equivalent a b = a==b

implies:: Bool->Bool->Bool
implies a b = if a==True&&b==False 
      then False
   else True


xDistance::(Int, Int) -> (Int, Int) -> Int
xDistance (a1,b1) (a2,b2) = abs(a1-a2)



invertX :: (Int, Int) -> (Int, Int)
invertX (x,y) = (x,-y)

isOnNeg2X :: (Int, Int) -> Bool
isOnNeg2X (x,y) =  y == (-2 * x)