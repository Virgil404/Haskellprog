module Lesson8 where

-- https://hoogle.haskell.org/
-- http://lambda.inf.elte.hu/CheatSheet.xml
-- ZH-ba használható

-- :! cls -> üríti a terminált
-- ctrl + c -> leállít egy végtelen loopot

inc :: Int -> Int
inc a = 1 + a

inc' :: Int -> Int
inc' = (+1)

--Lambdával

inc'' :: Int -> Int
inc'' = (\x -> x + 1)

-- filter
-- Egy olyan függvényt kap, ami igaz / hamis értéket ad vissza, és azokat az elemeket adja vissza a listába, amikre igazat adott

-- flip
-- Fog egy funkciót, és egy olyan funkciót ad vissza, ahol az első két bemenet felcserélődik

-- zipWith
-- Ez a funkció egy függvényt, és két listát kap bemenetül, a kimenet egy lista
-- A két listát egyesíti a megadott fügvényt alkalmazva

-- map
-- egy lista minden elemére alkalmaz egy függvényt

-- ISMÉTLÉS VÉGE --

-- foldl
-- Ez a funkció három paraméter kap, egy funkciót, egy kezdő értéket, és egy listát
-- Bal 'accumulator'

-- Sum függvény foldl

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- Itt a \acc x -> acc + x a funkció
-- 0 a kezdőérték
-- xs a lista amit "összehajtunk"

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> (x == y) || acc) False ys

elem'2 :: (Eq a) => a -> [a] -> Bool
elem'2 y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- A kezdőérték egy Bool érték -> a végső érték is Bool típusu lesz
-- \acc x -> if x == y then True else acc -> Ez átírja a False kezdőértéket, és kilép ha megtalálta
-- ha nem találta meg, megy tovább

-- foldr

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- Miért jók?
-- A Foldokat arra tudjuk használni, hogy egy listán egyszer végigmegyünk,
-- elementként, és az alapján ad vissza valamit.
-- foldl, foldr, map, filter

-- foldl1, foldr1 -> ugyan az mind a többi, csak nem kell kezdőérték, 
-- a lista első (vagy utolsó) eleme lesz a kezdőérték

-- Példák

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- scanl, scanr
-- Hasonló a foldl és foldr függvényekhez, de az accumulator értékeket egy listában adja vissza

-- Példák
-- scanl (+) 0 [3,5,2,1] 
-- scanr (+) 0 [3,5,2,1] 
-- scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1] 
-- scanl (flip (:)) [] [3,2,1] 

-- Egy függvény, ami megmondja, hogy hány elem kell ahhoz,
-- hogy a természetes számok gyökeinek összege 1000 felett legyen

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- sum $ map sqrt [1..131]
-- sum $ map sqrt [1..130]

-- $ használata (Function application)

-- ($) :: (a -> b) -> a -> b  
-- f $ x = f x

-- Precedencia: Az operátorok precedenciája azt határozza meg, hogy milyen "szorosan" köt össze két kifejezést.

-- Mire jó ?
-- Zárójelek elkerülése
-- Amikor $ van, akkor a jobb oldalán lévő kifejezésre használja a bal oldalon lévő függvényt

-- Példák
-- sum (map sqrt [1..130]) => sum $ map sqrt [1..130]
-- sqrt (3 + 4 + 9) => sqrt $ 3 + 4 + 9
-- sum (filter (> 10) (map (*2) [2..10])) => sum $ filter (> 10) $ map (*2) [2..10]
-- f (g (z x)) => f $ g $ z x (az előzőnek a függvényes leírása)


-- Function composition
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) f g x = f (g x)
--        VAGY
-- f . g = \x -> f (g x)

-- Mi a külömbség . és $ közt ?
-- (.) :: (b -> c) -> (a -> b) -> a -> c (Két egybemenetes függvényből, egy egybemenetes függvényt csinál)
-- ($) :: (a -> b) -> a -> b (Function application)

-- Mire jó ?
-- Példa:
-- f = g . h
-- f x == g (h x)

dotFunc :: Floating a => a -> a
dotFunc = (+5) . cos


ex1 :: (Floating a, Enum a) => p -> a -> a
ex1 f x = sqrt (sum (map (\i -> sin (exp (sqrt (i^2)))) [1..x]))

ex2 :: (Floating a, Enum a) => p -> a -> a
ex2 f x = sqrt $ sum $ map (\i -> sin $ exp $ sqrt $ i^2) [1..x]

ex3 :: (Floating a, Enum a) => p -> a -> a
ex3 f x = sqrt $ sum $ map (sin . exp . sqrt . (^2)) [1..x]

--Megmondja, hogy minden elem megfelel-e a feltételnek
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and . map p $ xs

-- Megmondja, hogy van-e elem ami megfelel a feltételnek
any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or . map p $ xs

-- f = g . h
-- f x == g (h x)

--Végtelenül alkalmazza az f függvényt x-re
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate f (f x)

--pl.:

iterateExample :: Num a => a -> [a]
iterateExample x = take 100 $ iterate (+2) x


--Elem funkció másképp
elem'' :: Eq a => a -> [a] -> Bool
elem'' x = any (==x)

-- Egy listából listát -> az eredeti listából addig veszi az elemeket, amíg teljesül a feltétel
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x        = x : takeWhile' p xs
    | otherwise  = []

-- Egy listából listát -> az eredeti listából addig dob el elemeket, amíg teljesül a feltétel
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []          = []
dropWhile' p (x:xs)
    | p x         = dropWhile' p xs
    | otherwise   = x:xs

-- Egy rendezett pár első tagját keresi, ahol megtalálja, a 2. tagot adja vissza
-- Maybe a -> Vagy egy a típusú valami, (Just a) vagy üres (Nothing)
-- Lekezeli a hibákat és kivételes eseteket, nem kell error

lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' k [] = Nothing
lookup' k ((a,b):xs)
     | k == a = Just b
     | otherwise = lookup' k xs

-- lookup' 3 [(1,"Alma"),(2,"Banán"),(3,"Citrom")]

-- Head függvény Maybe-vel

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- safeHead []
-- head []

-- safeHead használata lookuphoz
-- lookup' k = safeHead . map snd . filter ((==k) . fst)
-- lookup' k xs = safeHead (map snd (filter (\(x, _) -> x == k) xs))

-- snd -> a második elemét veszi egy tuple-nek
-- fst -> az első elemét veszi egy tuple-nek

--HASZNOS FUNKCIÓK LISTÁKRA
-- head -> a lista első eleme
-- tail -> a lista elemei az első nélkül
-- reverse -> a lista fordítottja
-- last -> a lista utolsó eleme
-- sum -> a listába lévő számok összege
-- elem -> megnézi, hogy a listának eleme-e egy megadott dolog
-- map -> egy lista minden elemére alkalmaz egy függvényt
-- filter -> egy listából kiválogatja azokat az elemeket, amik megfelelnek a megadott feltételnek
-- take -> a lista első elemeit adja meg, annyit, amennyit mi kérünk (végtelen listára is!)
-- takeWhile -> addíg veszi az elemeket, amíg megfelelnek egy megadott feltételnek
-- dropWhile -> addíg dobja el az elemeket, amíg megfelelnek egy megadott feltételnek
-- words -> egy stringből a space karakterek mentén szavak listáját hozza létre (pl.: words "abc def ghi" = ["abc","def","ghi"])
-- unwords -> a words fordítottja (pl.: unwords ["abc","def","ghi"] = "abc def, ghi")
-- maximum -> egy lista legnagyobb elemét adja vissza
-- minimum -> egy lista legkisebb elemét adja vissza
-- lookup -> egy rendezett pár első tagját keresi, ahol megtalálja, a 2. tagot adja vissza
-- iterate -> Végtelenül alkalmazza az f függvényt x-re
-- foldl, foldr -> 

--ELMÉLET
-- Rekurzió
-- Curryzés
-- Lambda
-- Kód kérdések
-- Előadás