module Ora10 where

    data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

    data Poinz = Point Float Float  deriving (Show)

    -- Rectangle értékei    Első két float a bal felső sarok 
    -- Második két pont a jobb alsó sarok kordinátái 


    --függveny ami felszint szamol a temakbol 

    surface:: Shape -> Float
    surface (Circle _ _ r) = pi*r^2
    surface (Rectangle  a b c d ) = (abs$ a-c)* (abs $  b-d)



    isListSmallerOrEqual4 :: [a] -> Bool
    isListSmallerOrEqual4 = xs go 4ll