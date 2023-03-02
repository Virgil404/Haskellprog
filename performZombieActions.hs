module PerformZombieActions where

import Data.List

type Coordinate = (Int, Int)
type Sun = Int

data Plant = Peashooter Int | Sunflower Int | Walnut Int | CherryBomb Int deriving(Eq,Show)

data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int deriving(Eq,Show)

data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)] deriving(Eq,Show)
    
defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 2

basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2


--VÁLTOZÓ NEVEKET MINDENKI NEVEZZE ÁT (napok, novenyek, zombik, ujNovenyek, ujZombik, zombiGy, zombiDbC, isVaulting, ujNovenyEletP, novenyEletP)
--VÁLTOZÓ NEVEKET MINDENKI NEVEZZE ÁT (napok, novenyek, zombik, ujNovenyek, ujZombik, zombiGy, zombiDbC, isVaulting, ujNovenyEletP, novenyEletP)
--VÁLTOZÓ NEVEKET MINDENKI NEVEZZE ÁT (napok, novenyek, zombik, ujNovenyek, ujZombik, zombiGy, zombiDbC, isVaulting, ujNovenyEletP, novenyEletP)


performZombieActions :: GameModel -> Maybe GameModel 
performZombieActions (GameModel napok novenyek zombik) = if any (\(c, z) -> snd c == 0) zombik then Nothing else Just (GameModel napok (ujNovenyek (GameModel napok novenyek zombik)) (ujZombik (GameModel napok novenyek zombik)) )
    where ujNovenyek :: GameModel -> [((Int, Int), Plant)]
          ujNovenyek (GameModel napok novenyek zombik) = (map (\(c, n) -> (c, (ujNovenyEletP n (novenyEletP n - (zombiDbC c zombik)))))) novenyek
          ujZombik :: GameModel -> [((Int, Int), Zombie)]
          ujZombik (GameModel napok novenyek zombik) = (map (\((x, y), z) -> if (isVaulting z) then (if ((x,y) `elem` (map fst novenyek)) then ((x, y-1), (zombiGy z)) else ((x, y-2), if ((x,(y-1)) `elem` (map fst novenyek)) then (zombiGy z) else z)) else (if not ((x,y) `elem` (map fst novenyek)) then ((x, y-1), z) else ((x, y), z)))) zombik
          zombiGy :: Zombie -> Zombie
          zombiGy (Vaulting eletero _) = (Vaulting eletero 1)
          zombiDbC :: Coordinate -> [((Int, Int), Zombie)] -> Int
          zombiDbC c zombik = length [x | x <- zombik, (fst (fst x)) == (fst c) && (snd (fst x)) == (snd c) && (not (isVaulting (snd x)))]

novenyEletP :: Plant -> Int
novenyEletP (Peashooter eletero) = eletero
novenyEletP (Sunflower eletero) = eletero
novenyEletP (Walnut eletero) = eletero
novenyEletP (CherryBomb eletero) = eletero

isVaulting :: Zombie -> Bool
isVaulting (Vaulting _ 2) = True
isVaulting _ = False

ujNovenyEletP :: Plant -> Int -> Plant
ujNovenyEletP (Peashooter _) eletero = (Peashooter eletero)
ujNovenyEletP (Sunflower _) eletero = (Sunflower eletero)
ujNovenyEletP (Walnut _) eletero = (Walnut eletero)
ujNovenyEletP (CherryBomb _) eletero = (CherryBomb eletero)


--VÁLTOZÓ NEVEKET MINDENKI NEVEZZE ÁT (napok, novenyek, zombik, ujNovenyek, ujZombik, zombiGy, zombiDbC, isVaulting, ujNovenyEletP, novenyEletP)
--VÁLTOZÓ NEVEKET MINDENKI NEVEZZE ÁT (napok, novenyek, zombik, ujNovenyek, ujZombik, zombiGy, zombiDbC, isVaulting, ujNovenyEletP, novenyEletP)
--VÁLTOZÓ NEVEKET MINDENKI NEVEZZE ÁT (napok, novenyek, zombik, ujNovenyek, ujZombik, zombiGy, zombiDbC, isVaulting, ujNovenyEletP, novenyEletP)