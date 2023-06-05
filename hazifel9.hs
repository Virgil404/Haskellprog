module Hazi9 where 

    foldTask :: Num a => [a] -> a
    foldTask = foldl (\acc x -> acc + x + 3) 0

    afterFunction :: (Num a, Ord a) => (a -> a) -> (a -> Bool) -> [a] -> [a]
    afterFunction f1 f2 xs = filter f2 (map f1 xs)

    applyToChosen :: (Ord a, Num a) => (a -> a) -> (a -> Bool) -> [a] -> [a]
    applyToChosen _ _ [] = []
    applyToChosen f p (x:xs)
        | p x       = f x : applyToChosen f p xs
        | otherwise = applyToChosen f p xs

