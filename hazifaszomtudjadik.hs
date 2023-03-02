module Homework where
import System.IO
import Data.List
import qualified Data.Map as Map

data Weather = Sunny {degree :: Int} | Cloudy {degree :: Int} | Rainy {degree :: Int, fall :: Int} | Snowy {degree :: Int, fall :: Int} deriving(Eq, Show, Read)
d1 :: Weather
d1 = Sunny 18
d2 :: Weather
d2 = Cloudy 14
d3 :: Weather
d3 = Rainy 12 20
d4 :: Weather
d4 = Snowy (-9) 21

goodWeather :: Weather -> Bool
goodWeather (Rainy _ _) = False
goodWeather (Snowy _ _) = False
goodWeather (Sunny a) = a > 16
goodWeather (Cloudy b) = b > 18

type Days = [Weather] 
days1 :: Days
days1 = [d1,d2,d3,d4]

subZero :: Weather -> Bool
subZero (Snowy x _) = x < 0
subZero (Rainy x _) = x < 0
subZero (Cloudy y) = y < 0
subZero (Sunny y) = y < 0

freezingDays :: Days -> Days
freezingDays days = filter subZero days