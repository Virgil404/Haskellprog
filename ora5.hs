module Lesson5 where 



    --mintaillesztés 

    -- [] üres lista
    -- [x] egy elemű list
    -- (xs:x) legalább egy  elemű lista (x első xs többi elem)
    -- (xs:x) legalább egy elemú lista 
    -- (v)

    notTuple:: (Bool,Bool)-> (Bool,Bool)
    notTuple (s,x)=(not s, not x )


    chekcSingleton :: [a]-> Bool
    chekcSingleton [x] = True
    chekcSingleton (x:xs) = False



    notList:: [Bool]->[Bool]
    notList []=[]
    notList (x:xs)=not x: notList xs