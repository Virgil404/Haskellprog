module ChatgptTest where 
data Date = Date Int Int Int
    deriving (Show, Eq)



christmas2022 :: Date
christmas2022 = Date 2022 12 24
hatarido :: Date
hatarido = Date 2022 12 16
santa :: Date
santa = Date 2022 12 6
lastChristmas :: Date
lastChristmas = Date 2021 12 24

isWinterHoliday2022 :: Date -> Bool
isWinterHoliday2022 (Date year month day) =
    year == 2022 && (month, day) `elem` [(12, 6), (12, 24), (12, 25), (12, 26), (12, 31)]