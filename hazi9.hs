module Hazi9 where 
import Data.List (intersperse, transpose)



monogram :: [Char] -> [Char]
monogram x = intersperse '.' (map head $ words x)

biggestNum :: (Ord a, Foldable t) => t [a] -> a
biggestNum list = maximum (concat list)

mtxTranspose:: Num a => [[a]]->[[a]]
mtxTranspose = transpose  
