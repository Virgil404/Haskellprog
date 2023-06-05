module Hazi11 where

data Weather
  = Sunny Int
  | Cloudy Int
  | Rainy Int Int
  | Snowy Int Int
  deriving (Eq)  -- Eq instance hozzáadása

-- A további definíciók és függvények maradhatnak változatlanok


d1 :: Weather
d1 = Sunny 18

d2 :: Weather
d2 = Cloudy 14

d3 :: Weather
d3 = Rainy 12 20

d4 :: Weather
d4 = Snowy (-9) 21


goodWeather :: Weather -> Bool
goodWeather (Sunny temp) = temp >= 16
goodWeather (Cloudy temp) = temp >= 18
goodWeather _ = False


type Days = [Weather]

days1 :: Days
days1 = [d1, d2, d3, d4]




freezingDays :: Days -> Days
freezingDays days = filter isFreezing days
  where
    isFreezing (Sunny temp) = temp < 0
    isFreezing (Cloudy temp) = temp < 0
    isFreezing (Rainy temp _) = temp < 0
    isFreezing (Snowy temp _) = temp < 0
