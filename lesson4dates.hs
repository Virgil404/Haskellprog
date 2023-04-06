module Dates where 

mdyToDmy::(String, Int, Int )->(Int,String,Int)
mdyToDmy (m,d,y)=(d,m,y)

getdayDMY::(Int,String,Int)->Int
getdayDMY (d,_,_)=d

getyearDMY ::(Int,String,Int)->Int
getyearDMY (_,_,y)=y

