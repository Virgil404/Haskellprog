module Hazi4 where 


task1 :: [Int] -> [Int]
task1 a= [x|x<-a, x `mod`3==0,even x, x>10 ,x<100]
task2 :: String -> String
task2 a=[x|x<-a,x/='y',x/='Y']

task3 :: [Int] -> [Int]
task3 [] = error "ures lista"
task3 a = [3*x|x<-a]

task4 :: [Int] -> [Int] -> [Int]
task4 a b = [x*y| x<-a ,y<-b, x/=0,y/=0]