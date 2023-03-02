-- Plants vs. Zombies
module Pvz where
    -- felhasznált külső modulok: Data.List
    import Data.List
    -- 1. definiált adattípusok, játék modellezése: 
    -- 1.1 játékos napjainak a száma és a pálya egy adott pontja két koordinátával megadva
    -- koordináták (koordinata1,koordinata2) : egész
    type Coordinate = (Int, Int)
    -- napok száma : egész
    type Sun = Int
    -- 1.2 növények és zombik: a játékos a növényekkel védekezik a zombik ellen
    -- többféle növény van: Peashooter, Sunflower, Walnut és CherryBomb
    -- minden növényről az életpontjainak a számát (egész szám) tároljuk
    data Plant = Peashooter Int | Sunflower Int |Walnut Int | CherryBomb Int deriving(Eq,Show)
    -- Zombik: 
    -- többféle zombi van: Basic, Conehead, Buckethead és Vaulting
    -- zombik életpontjai és mozgási sebessége : mindkettő egész szám
    data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int deriving(Eq,Show)
    -- 1.3 játék modellje: tartalmazza a játékos napjainak a számát, és a növények és zombik két koordinátával megadott pozícióját
    data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)] deriving(Eq,Show)
    -- 2. növényvásárlás: a játékos egy adot koordinátára növényt próbál venni
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


    

    {-
    tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
    tryPurchase (GameModel numSuns plants zombies) coord noveny = if (outOfBoard||plantOnCoord||fewSuns) then Nothing else Just (GameModel numSuns (plants++[(coord, noveny)]) zombies) where
        -- outOfBoard: játéktéren kívül próbál vásárolni a játékos
        outOfBoard = if((any (>4) coord)||(any (>11) coord)||(any (<0) coord)) then True else False
        -- plantOnCoord: a megadott koordinátán már van növény, ezért oda nem lehet már venni másik növényt
        plantOnCoord = if((lookup coord plants)/=Nothing) then True else False
        -- fewSuns: nem lehet megvenni a növényt, mert a játékosnak nincs elég napja
        fewSuns = if(numSuns<(price noveny)) then True else False where
            -- price: növényfajtákhoz az árukat rendeli
            price :: Plant -> Sun
            price (CherryBomb _) = 150
            price (Peashooter _) = 100
            price (Walnut _) = 50
            price (Sunflower _) = 50
-- #? 2. tesztesettel nem az elvárt kimenetet adja
    -- teszteset: tryPurchase (GameModel 50 [] []) (0,0) defaultWalnut == Just (GameModel 0 [((0,0), defaultWalnut)] [])
    -- elvárt helyett kapott kimenet: Just (GameModel 50 [((0,0),Walnut 15)] [])
    -- 3. zombik elhelyezése: zombik mindig a sorok végén jelennek meg
    placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
    placeZombieInLane (GameModel numSuns plants zombies) zombie lane = if(outOfLane||existsZombie) then Nothing else Just (GameModel numSuns plants (zombies++[((lane,11), zombie)])) where
        -- outOfLane: ellenőrző függvény - jó sávszámot (0 és 4 közötti egész számot) adtunk-e meg
        outOfLane= lane>4
        -- existsZombie: ellenőrző függvény, megadja, hogy van-e zombi a sor végén
        existsZombie = (lookup (lane,11) zombies)/=Nothing
    -- #? 4. tesztesettel nem az elvárt kimenetet adja
    -- teszteset: placeZombieInLane (GameModel 0 [] [((0,3), coneHead)]) basic 0 == Just (GameModel 0 [] [((0,11), basic),((0,3), coneHead)])
    -- elvárt kimenet helyet kapott kimenet: Just (GameModel 0 [] [((0,3),Conehead 10 1),((0,11),Basic 5 1)])
    queryPlants :: GameModel -> [(Coordinate,Plant)]
    queryPlants (GameModel _ [] _)  = []
    queryPlants (GameModel _ (x:xs) _)  = (x:xs)

    queryZombies :: GameModel -> [(Coordinate,Zombie)]
    queryZombies (GameModel _ _ [])  = []
    queryZombies (GameModel _ _ (x:xs))  = (x:xs)

    querySuns :: GameModel -> Sun
    querySuns (GameModel x _ _) = x

    novenyPont :: Plant -> Int
    novenyPont (Peashooter x) = x
    novenyPont (Sunflower x) = x
    novenyPont (Walnut x) = x
    novenyPont (CherryBomb x) = x
    
    zombiPont :: Zombie -> Int
    zombiPont (Basic x _) = x
    zombiPont (Conehead x _) = x
    zombiPont (Buckethead x _) = x
    zombiPont (Vaulting x _) = x

    cleanBoard :: GameModel -> GameModel
    cleanBoard game = (GameModel (querySuns game) novenyek zombik) where
        novenyek=[n | n <- (queryPlants game),(novenyPont (snd n))>0]
        zombik=[z | z <- (queryZombies game),(zombiPont (snd z))>0]
        -}