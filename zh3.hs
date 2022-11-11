module ZH3 where  

-- 1 kérdés névtelen függvényeket tudunk vele csinálni 
-- 2. 


-- 1 feladat 
fourthPowerEvens :: [Int]
fourthPowerEvens =[x^4 | x <- [1..], odd x]
-- 2 feladat
numSins :: [Float] -> Int
numSins a = foldr (\x acc -> if sin x > 0 then acc+1 else acc) 0 a

-- 3 feladat
smallMult :: [(Int, Int, Int)] -> [Int]
smallMult [] = []
smallMult ((a,b,c):xs) = filter (<100) (abs(a) * abs(b) * abs(c) : smallMult xs)