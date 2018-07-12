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


data WherePenguinsLive =
        Galapagos
      | Antarctica
      | Australia
      | SouthAfrica
      | SouthAmerica
      deriving (Eq, Show)

--product type. Penguin is a type with only one value, Peng  which contains a WherePenguinsLive value.
data Penguin =
        Peng WherePenguinsLive
        deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereItLives) = whereItLives

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
        (galapagosPenguin p) || (antarcticPenguin p)


-- Pattern matching on tuples
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a,b) (c,d) = ((b, d), (a, c))


-- the members of the tuple must be of the same type bc. (+) is a->a->a
-- addEmUp2 (1, 2) = 3
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

-- fst3 (1, 2, 3) = 1
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- third3 (1, 2, 3) = 3
third3 :: (a, b, c) -> c
third3 (_, _, z) = z