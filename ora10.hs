module Ora10 where 

 data Bool = False| True

 -- sjaat osztaly  

 data Shape = Circle Float Float Float | Rectangle Float Float Float Float  deriving(Show)

 surface :: Shape -> Float 
 surface (Circle _ _ r)= pi * r^2 
 surface (Rectangle x1 y1 x2 y2) = (abs $ x2-x1 )*(abs $ y2-y1 )

 primes :: [Integer]
 primes = 2: 3: sieve (tail primes) [5,7..]
  where 
    sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]  
                                -- or:  filter ((/=0).(`rem`p)) t
                  where (h,~(_:t)) = span (< p*p) xs