module Lesson4 where 

    countThrees:: [Int]-> Int
    countThrees lista = sum([ 1| x<-lista, x==3 ])

    