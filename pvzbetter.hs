module Pvz where
    import Data.List

    type Coordinate = (Int, Int)

    type Sun = Int


    data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int deriving(Eq,Show)

    data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)] deriving(Eq,Show)


    data Plant = Peashooter Int | Sunflower Int |Walnut Int | CherryBomb Int deriving(Eq,Show)

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




    getZombieNam :: Zombie -> String
    getZombieNam (Basic _ _) = "Basic"

    getZombieNam (Buckethead _ _) = "Buckethead"
    getZombieNam (Conehead _ _) = "Conehead"
    getZombieNam (Vaulting _ _) = "vaulting"


    getPointZombie :: Zombie -> Int
    getPointZombie (Basic x _) = x
    getPointZombie (Conehead x _) = x
    getPointZombie (Buckethead x _) = x
    getPointZombie (Vaulting x _) = x

    getPlantName :: Plant -> String
    getPlantName (Peashooter _) = "Peashooter"
    getPlantName (Sunflower _) = "Sunflower"

    getPlantName (Walnut _) = "Walnut"
    getPlantName (CherryBomb _) = "CherryBomb"

    getZombies :: GameModel -> [(Coordinate,Zombie)]
    getZombies (GameModel _ _ [])  = []
    getZombies (GameModel _ _ (x:xs))  = (x:xs)




    getPointPlant :: Plant -> Int
    getPointPlant (Peashooter x) = x
    getPointPlant (Sunflower x) = x
    getPointPlant (Walnut x) = x
    getPointPlant (CherryBomb x) = x

    getZombieSpeed :: Zombie -> Int
    getZombieSpeed (Basic _ x) = x
    getZombieSpeed (Conehead _ x) = x
    getZombieSpeed (Buckethead _ x) = x
    getZombieSpeed (Vaulting _ x) = x

    getSunpos :: GameModel -> Sun
    getSunpos (GameModel x _ _) = x


    getPlant :: GameModel -> [(Coordinate,Plant)]
    getPlant (GameModel _ [] _)  = []
    getPlant (GameModel _ (x:xs) _)  = (x:xs)


    tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel

    tryPurchase (GameModel numSuns plants zombies) doordinate plant = if ((isInPlayarea doordinate)||enoughSun||isCordanteOccupied) then Nothing else Just (GameModel (numSuns-(getPrice plant)) (plants++[(doordinate, plant)]) zombies) where
        isInPlayarea :: Coordinate -> Bool
        isInPlayarea (a,b) = (a>4)||((a>11)||(b>11))||((a<0)||(b<0))
        isCordanteOccupied = if((lookup doordinate plants)/=Nothing) then True else False
        enoughSun = if(numSuns<(getPrice plant)) then True else False
        getPrice :: Plant -> Sun
        getPrice n
            | (((getPlantName n)=="Sunflower")||((getPlantName n)=="Walnut")) = 50
            | ((getPlantName n)=="Peashooter") = 100
            | ((getPlantName n)=="CherryBomb") = 150
    placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
    placeZombieInLane (GameModel numSuns plants zombies) zombie lane = if(notInplayarea||existsZombie) then Nothing else Just (GameModel numSuns plants ([((lane,11), zombie)]++zombies)) where
        notInplayarea= (lane>4)||(lane<0)
        existsZombie = (lookup (lane,11) zombies)/=Nothing


    newplanthealth :: Plant -> Int -> Plant
    newplanthealth (Peashooter _) hp = (Peashooter hp)
    newplanthealth (Sunflower _) hp = (Sunflower hp)
    newplanthealth (Walnut _) hp = (Walnut hp)
    newplanthealth (CherryBomb _) hp = (CherryBomb hp)

    ph :: Plant -> Int
    ph (Peashooter hp) = hp
    ph (Sunflower hp) = hp
    ph (Walnut hp) = hp
    ph (CherryBomb hp) = hp

    zvaulting :: Zombie -> Bool
    zvaulting (Vaulting _ 2) = True
    zvaulting _ = False

    performZombieActions :: GameModel -> Maybe GameModel
    performZombieActions (GameModel suns plants zombies) = if any (\(c, z) -> snd c == 0) zombies then Nothing else Just (GameModel suns (newPlants (GameModel suns plants zombies)) (newZombies (GameModel suns plants zombies)) )
        where newPlants :: GameModel -> [((Int, Int), Plant)]
              newPlants (GameModel suns plants zombies) = (map (\(c, n) -> (c, (newplanthealth n (ph n - (zRDC c zombies)))))) plants
              newZombies :: GameModel -> [((Int, Int), Zombie)]
              newZombies (GameModel suns plants zombies) = map (\((x, y), z) -> if (zvaulting z) then (if ((x,y) `elem` (map fst plants)) then ((x, y-1), (zombieW z)) else ((x, y-2), if ((x,(y-1)) `elem` (map fst plants)) then (zombieW z) else z)) else (if not ((x,y) `elem` (map fst plants)) then ((x, y-1), z) else ((x, y), z))) zombies
              zombieW :: Zombie -> Zombie
              zombieW (Vaulting hp _) = (Vaulting hp 1)
              zRDC :: Coordinate -> [((Int, Int), Zombie)] -> Int
              zRDC c zombies = length [x | x <- zombies, (fst (fst x)) == (fst c) && (snd (fst x)) == (snd c) && (not (zvaulting (snd x)))]

    cleanBoard :: GameModel -> GameModel
    cleanBoard game = (GameModel (getSunpos game) plants zombies) where
        plants=[n | n <- (getPlant game),(getPointPlant (snd n))>0]
        zombies=[z | z <- (getZombies game),(getPointZombie (snd z))>0]

