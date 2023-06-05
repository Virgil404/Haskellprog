module Hazi6 where 
    tuplesAdd :: [(Int, Int)] -> [Int]
    tuplesAdd tuples = [x + y | (x,y) <- tuples]

    nullDivByThree :: Int -> Int
    nullDivByThree n
        | n `mod` 3 == 0 = 0
        | otherwise = n

    nullList :: [Int] -> [Int]
    nullList xs = [nullDivByThree x | x <- xs]
    
    
    nullListTuples :: [(Int,Int)] -> [Int]
    nullListTuples tuples = nullList (tuplesAdd tuples)


    test7 :: Int -> Int
    test7 num = if mod num 2 /= 0 then 0 else num       