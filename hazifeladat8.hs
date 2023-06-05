module Hazi8 where 


    dropWord :: String -> String
    dropWord str = unwords $ tail $ words str

    task3 :: Int -> Int
    task3 x = x * 2 + 3

    task3Sums :: Int
    task3Sums = length $ takeWhile (<1000) $ scanl1 (+) $ map task3 [0..]

    listOfSquareNums :: [Int]
    listOfSquareNums = [x*x | x <- [1..]]

    isSquareNum :: Int -> Bool
    isSquareNum n = let root = floor $ sqrt $ fromIntegral n
                in n == root*root


    selectDiv8 :: Integral a => [a] -> [a]
    selectDiv8 xs = takeWhile (\x -> x `mod` 8 == 0) xs
