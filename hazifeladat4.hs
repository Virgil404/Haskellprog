module Hazi4 where 

    fahrToCel :: Float -> Float
    fahrToCel f = (f - 32) / 1.8

    isHot :: Float -> Bool
    isHot f = let c = fahrToCel f in c > 30

    goodWeather :: String -> Float -> Bool
    goodWeather "Clear" f = isHot f
    goodWeather _ _ = False

