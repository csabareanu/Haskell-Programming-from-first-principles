module DataVsType where

-- type constructors -- compile-time

-- -------------------- phase separation

-- data constructors -- runtime

-- When data constructors take arguments, those arguments refer to other types.

data Price =
    Price Integer deriving (Eq, Show)

data Manufacturer =
      Mini
    | Mazda
    | Tata
    deriving (Show, Eq)

data Airline =
      PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Show, Eq)

data Vehicle =
    Car Manufacturer Price
    | Plane Airline
    deriving (Show, Eq)
