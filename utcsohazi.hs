module Utcsohazi where 
import Data.Char
import Data.ByteString (split)
import Data.Text.Lazy (splitOn)
import Control.Arrow (Arrow(first))
--import Data.Char (isAlpha, isAlphaNum)

data Vehicle = Truck { manufacturer :: String,
                       weight :: Int,
                       maxLoad :: Int,
                       fuelConsumption :: Float,
                       mileage :: Int,
                       available :: Bool,
                       id :: Int
                     }
             | Van { manufacturer :: String,
                     maxLoad :: Int,
                     fuelConsumption :: Float,
                     available :: Bool,
                     id :: Int
                   }

tr1 :: Vehicle
tr1 = Truck "Iveco" 7900 15000 25.5 425315 True 87

tr2 :: Vehicle
tr2 = Truck "Mercedes" 12100 12000 38.5 243103 True 44

tr3 :: Vehicle
tr3 = Truck "Mercedes" 13600 8000 23.5 156078 False 63

vn1 :: Vehicle
vn1 = Van "Renault" 1500 10.9 True 31

vn2 :: Vehicle
vn2 = Van "Ford" 1600 11.7 False 98


efficientTruck :: Vehicle -> Bool
efficientTruck (Truck _ _ maxLoad fuelConsumption _ _ _) = fuelConsumption * fromIntegral maxLoad / 100 < 4000
efficientTruck _ = False


pickVehicle :: [Vehicle] -> Int -> Int
pickVehicle [] _ = error "No suitable vehicle found."
pickVehicle ((Truck _ _ maxLoad _ _ available id):vs) weight
    | weight <= maxLoad && available = id
    | otherwise = pickVehicle vs weight
pickVehicle ((Van _ maxLoad _ available id):vs) weight
    | weight <= maxLoad && available = id
    | otherwise = pickVehicle vs weight



splitQuadruple :: (a, b, c, d) -> ((a, b), (c, d))
splitQuadruple (w, x, y, z) = ((w, x), (y, z))

dist1 :: Num a => a -> a -> a
dist1 a b = abs( a-b)

kroeneckerDelta :: Eq a => a -> a -> Int
kroeneckerDelta a b 
 | a == b = 1
 | otherwise = 0

freq :: Eq a => a -> [a] -> Int
freq _ [] = 0
freq x (y:ys)
    | x == y    = 1 + freq x ys
    | otherwise = freq x ys


hasUpperCase :: String -> Bool
hasUpperCase [] = False
hasUpperCase (x:xs)
    | isUpper x = True
    | otherwise = hasUpperCase xs

identifier2 :: String -> Bool
identifier2 [] = False
identifier2 (x:xs)
    | not (isAlpha x) = False
    | otherwise = all validChar xs
  where
    validChar c = isAlphaNum c || c == '_'

which :: ([Char], [Char], [Char]) -> Char -> Int
which (a, b, c ) i 
     | i`elem` a = 1 
     | i `elem` b = 2 
     | i `elem` c = 3 
     | otherwise = 0


matches :: (Int, Int) -> (Int, Int) -> Bool
matches (a1, b1) (a2,b2) = a1==b2 || b1 == a2


toUpperCase :: String -> String
toUpperCase [] = []
toUpperCase (x:xs) 
   | x == '?'  = x : xs
   | otherwise = toUpper x : xs

numeric :: String -> Int
numeric []=0
numeric (x:xs)
 | x =='r' = 4 + numeric xs 
 | x == 'w' = 2 + numeric xs 
 | x == 'x' = 1 + numeric xs 
 | otherwise = 0 ; 


hasLongWord :: Int -> String -> Bool
hasLongWord n str = any (\word -> length word >= n) (words str)

align :: Int -> String -> String
align l text
  | length text >= l = text
  | length text <=0 = text
  | otherwise = align l(" " ++ text)

removeAccents :: String -> String
removeAccents = map replaceAccents
  where
    replaceAccents c = case c of
      'á' -> 'a'
      'é' -> 'e'
      'í' -> 'i'
      'ó' -> 'o'
      'ö' -> 'o'
      'ő' -> 'o'
      'ú' -> 'u'
      'ü' -> 'u'
      'ű' -> 'u'
      _   -> c


data RPS = Rock | Paper | Scissors deriving (Eq, Show)

beats :: RPS -> RPS
beats  Rock =  Scissors
beats Scissors = Paper 
beats Paper = Rock 

firstBeats :: [RPS] -> [RPS] -> Int
firstBeats []_ = 0
firstBeats _[] = 0 
firstBeats (x:xs) (x2:xs2)
 |  beats x == x2 = 1+ firstBeats xs xs2 
 |  otherwise = firstBeats xs xs2 