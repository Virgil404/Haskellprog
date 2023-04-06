module Hazi5 where

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

and3 :: Bool -> Bool -> Bool -> Bool
and3 a b c = a && b && c

xor3 :: Bool -> Bool -> Bool -> Bool
xor3 a b c = (a && not (b || c)) || (b && not (a || c)) || (c && not (a || b))
