-- 2022-12-05
module Hf12 where
    
    data Date = Date {year::Int, month::Int, day::Int} deriving(Eq, Ord, Read, Show)
    
    christmas2022 :: Date
    christmas2022 = Date 2022 12 24
    hatarido :: Date
    hatarido = Date 2022 12 16
    santa :: Date
    santa = Date 2022 12 6
    lastChristmas :: Date
    lastChristmas = Date 2021 12 24
        
    isWinterHoliday2022 :: Date -> Bool
    isWinterHoliday2022 datum = if(((year datum)==2022)&&((month datum)==12)&&((day datum)`elem`fests)) then True else False where fests =([6,31]++[24..26])
    
    timeBetween :: Date -> Date -> (Int,Int,Int)
    timeBetween d1 d2 = ((year res),(month res),(day res)) where
        res :: Date
        res = if (d1>d2) then (calc d1 d2) else (calc d2 d1)
        calc :: Date -> Date -> Date
        calc a b = Date ((year a)-(year b)) ((month a)-(month b)) ((day a)-(day b))
