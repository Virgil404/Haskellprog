module Lesson8 where 
    --Magasabb Rendű függvények 
    --Teszteléshez

addThree :: Num a => a ->a
addThree x= x+3

kivonas :: Num a => a->a->a
kivonas x y = x-y 

applyTwice :: Num a => (a->a)-> a->a
applyTwice  f x =  f (f x)

addSix  :: Num a => a ->a
addSix x = applyTwice addThree x 



