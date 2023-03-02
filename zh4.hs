module ZH4 where 
    -- 1 feladat  Enum felsorolás Bounded azt jelenti hogy a típusnak van alsó és felső határa 

    --- Zaccomer Virgil HGWN55  

    data Consumable = Food {name :: String, calory ::Float , fat :: Float, crobonhydrate:: Float,  protein :: Float} | Drink { name :: String, calory :: Float,  sugar :: Int}  deriving (Show, Read, Eq, Ord)
    
    data Car = Petrol {consumption :: Float } | Diesel{consumption :: Float } | Electric {consumption :: Float} |Hybrid {fuelconsumption :: Float, electricconsumption::Float} 

    penauts :: Consumable
    penauts = Food "Penaut" 550 60 80 10

    chicken :: Consumable
    chicken = Food "Chicken" 250 10 15 45

    potato :: Consumable
    potato = Food "Potato" 310 90 5 8

    bacon :: Consumable
    bacon = Food "Bacon" 450 10 60 30

    milkshake :: Consumable
    milkshake = Drink "Shake" 800 50

    water :: Consumable
    water = Drink "Water" 0 0



    highProtein :: Consumable -> Bool
    highProtein (Food _ calory _ _ protein) = calory<400 &&  protein >30
    highProtein _ = False


    pickHealthyFood :: [Consumable] -> [Consumable]
    pickHealthyFood [] = []
    pickHealthyFood (x:xs)
    | (calory x) < 400 && (protein x) >= 30 && (fat x) < 15 = pickHealthyFood xs
    | otherwise = pickHealthyFood xs

   -- healthyfood :: 

   -- pickHealthyFood :: [Consumable] -> [Consumable]
    c1 :: Car
    c1 = Petrol 7.4

    c2 :: Car
    c2 = Diesel 6.3

    c3 :: Car
    c3 = Electric 10.2

    c4 :: Car
    c4 = Hybrid 5.5 6.2


    travelCost :: Car -> Float -> Float
    travelCost (Petrol consumption) a = consumption*a/100*480
    travelCost (Diesel consumption) a = consumption*a/100*550
    travelCost (Electric consumption) a = consumption*a/100*150
    travelCost (Hybrid  fuelconsumption electricconsumption) a = fuelconsumption*a/100/2*150 + electricconsumption*a/100/2*480


