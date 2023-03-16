module Hazi2 where 

    toTuple :: Int -> Int -> (Int, Int)
    toTuple x y = (x, y)

    tupleAverage :: (Double, Double, Double) -> Double
    tupleAverage (x, y, z) = (x + y + z) / 3.0

    addToTuple :: Int -> (Int, Int, Int) -> (Int, Int, Int)
    addToTuple n (x, y, z) = (x+n, y+n, z+n)
