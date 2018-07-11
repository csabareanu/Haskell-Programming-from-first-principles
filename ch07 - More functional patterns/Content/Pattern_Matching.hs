module Pattern_Matching where

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False -- universal pattern (anything else case)

-- Patterns must be ordered from most specific to least specific
-- Incomplete pattern matches applied to data they don't handle will return bottom and this will throw an exception.

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber


printUser :: User -> IO()
printUser UnregisteredUser = putStrLn "Unregistered User"
printUser (RegisteredUser (Username name)
                         (AccountNumber acctNr))

        = putStrLn $ name ++ " " ++ show acctNr

user = printUser (RegisteredUser (Username "John") (AccountNumber 123456)) -- John 123456
