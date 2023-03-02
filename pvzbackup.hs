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



    getPlantName :: Plant -> String
    getPlantName (Peashooter _) = "Peashooter"
    getPlantName (Sunflower _) = "Sunflower"
    getPlantName (Walnut _) = "Walnut"
    getPlantName (CherryBomb _) = "CherryBomb"

    getZombieNam :: Zombie -> String
    getZombieNam (Basic _ _) = "Basic"
    getZombieNam (Buckethead _ _) = "Buckethead"
    getZombieNam (Conehead _ _) = "Conehead"
    getZombieNam (Vaulting _ _) = "Vaulting"

    getPointZombie :: Zombie -> Int
    getPointZombie (Basic x _) = x
    getPointZombie (Conehead x _) = x
    getPointZombie (Buckethead x _) = x
    getPointZombie (Vaulting x _) = x


    
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
    
    getPlant :: GameModel -> [(Coordinate,Plant)]
    getPlant (GameModel _ [] _)  = []
    getPlant (GameModel _ (x:xs) _)  = (x:xs)

    getSunpos :: GameModel -> Sun
    getSunpos (GameModel x _ _) = x

    tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
    tryPurchase (GameModel numSuns plants zombies) doordinate plant = if ((isInPlayarea doordinate)||isCordanteOccupied||enoughSun) then Nothing else Just (GameModel (numSuns-(getPrice plant)) (plants++[(doordinate, plant)]) zombies) where
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
    placeZombieInLane (GameModel numSuns plants zombies) zombie lane = if(outOfLane||existsZombie) then Nothing else Just (GameModel numSuns plants ([((lane,11), zombie)]++zombies)) where
        outOfLane= (lane>4)||(lane<0)
        existsZombie = (lookup (lane,11) zombies)/=Nothing
    performZombieActions :: GameModel -> Maybe GameModel
    performZombieActions game = if(any (endBoard) (fmap (fst) newZombies)) then Nothing else Just (GameModel (getSunpos game) newPlants newZombies) where
        endBoard :: Coordinate -> Bool
        endBoard (a,b) = (a>4)||((a>11)||(b>11))||((a<0)||(b<0))
        vanNoveny :: Coordinate -> Bool
        vanNoveny doordinate = (lookup doordinate (getPlant game))/=Nothing
        vanZombi :: Coordinate -> Bool
        vanZombi doordinate = (lookup doordinate (getZombies game))/=Nothing
        fullVaultings = (map (fst) [x | x <- (getZombies game),((getZombieSpeed (snd x))==2&& (getZombieNam (snd x))=="Vaulting")])
        onlyPlants = (map (fst) [x | x <- (getPlant game),(vanZombi (fst x))==False])
        newZombies = (map (zombiMove) (getZombies game)) where
            zombiMove :: (Coordinate,Zombie) -> (Coordinate,Zombie)
            zombiMove z
                | (((fst z)`elem`fullVaultings)&&(vanNoveny (fst z))) = ((ujvcoord (fst z)),(ujvzombi (snd z)))
                | (((fst z)`elem`fullVaultings)&&(vanNoveny (fst z))==False) = ((ujcoord (fst z)),(snd z))
                | (((fst z)`elem`fullVaultings)==False&&(vanNoveny (fst z))) = z
                | (((fst z)`elem`fullVaultings)==False&&(vanNoveny (fst z))==False) = ((ujcoord (fst z)),(snd z))
                where
                    ujcoord :: Coordinate -> Coordinate
                    ujcoord (a,b) = (a,(b-(getZombieSpeed (snd z))))
                    ujvcoord :: Coordinate -> Coordinate
                    ujvcoord (a,b) = (a,(b-1))
                    ujvzombi :: Zombie -> Zombie
                    ujvzombi zombi = (Vaulting (getPointZombie zombi) ((getZombieSpeed zombi)-1))
        newPlants = (map (zombiAttack) (getPlant game)) where
            zombiAttack :: (Coordinate,Plant) -> (Coordinate,Plant)
            zombiAttack x
                | (((fst x)`elem`fullVaultings)||((fst x)`elem`onlyPlants)) = x
                | otherwise = ((fst x),(ujnoveny (snd x))) where
                    ujnoveny :: Plant -> Plant
                    ujnoveny (Peashooter points) = (Peashooter (points-1))
                    ujnoveny (Sunflower points) = (Sunflower (points-1))
                    ujnoveny (Walnut points) = (Walnut (points-1))
                    ujnoveny (CherryBomb points) = (CherryBomb (points-1))
    cleanBoard :: GameModel -> GameModel
    cleanBoard game = (GameModel (getSunpos game) novenyek zombik) where
        novenyek=[n | n <- (getPlant game),(getPointPlant (snd n))>0]
        zombik=[z | z <- (getZombies game),(getPointZombie (snd z))>0]
   
