module Hazivalamennyi where 

    -- 1. feladat: 
selectDiv8 :: Integral a => [a] -> [a]
selectDiv8 xs = takeWhile (\x -> x`mod`8==0) xs

dropWord :: String -> String
dropWord xs = (unwords(tail(words(xs))))

task3 :: Int -> Int
task3 x = x*2+3

task3Sums :: Int
task3Sums = length (takeWhile (<1000) (scanl1 (+) (map task3[1..]))) + 1

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

plusthree ::Num a => a ->a
plusthree a = a+3

foldTask :: (Num a) => [a] -> a
foldTask xs = foldl (\acc x -> acc + x+3) 0 xs



-- utolsó feladat: 
-- négyzetszámok végtelen lista: 
listOfSquareNums :: [Int]
listOfSquareNums = map (\x -> x*x) [1..]
-- eldöntés: egy megadott szám négyzetszám-e
isSquareNum :: Int -> Bool
isSquareNum x = x == (head $ dropWhile (<x) listOfSquareNums)


