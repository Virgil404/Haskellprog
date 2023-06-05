module Hazi10 where 

    data User = User{ username :: String,password :: String,email :: String ,balance :: Int, pin :: Int} deriving (Show)

    user1 :: User
    user1 = User
        { username = "user1"
        , password = "pass123"
        ,email = "exampleemail"
        , balance = 12500
        , pin = 1111
        }

    user2 :: User
    user2 = User
        { username = "user2"
        , password = "123password"
        , email = "example2email"
        , balance = 150000
        , pin = 4578
        }

    user3 :: User
    user3 = User
        { username = "user3"
        , password = "123passw0rd"
        , email = "example22email"
        , balance = 3150000
        , pin = 4578
        }

    transactionPossible :: User -> Int -> Bool
    transactionPossible user amount = amount < balance user

   -- login :: User -> String -> String -> Int -> Bool
    --login user name pass pinCode = username user == name && password user == pass && pin user == pinCode

    login :: User -> String -> String -> Int -> Bool
    login user name pass pinCode = username user == name && password user == pass && pin user == pinCode
