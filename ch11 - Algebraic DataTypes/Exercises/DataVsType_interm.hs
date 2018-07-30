module DataVsType_interm where

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

data Size =
      Big
    | Medium
    | Small
    | Tiny


data Vehicle2 =
      Car2 Manufacturer Price
    | Plane2 Airline Size
-- For these exercises, we’ll use the datatypes defined in the above section.
-- It would be good if you’d typed them all into a source file already, but
-- if you hadn’t please do so now. You can then define some sample data
-- on your own, or use these to get you started:

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- 1. What is the type of myCar?
-- myCaR :: Vehicle


-- 2. Given the following, define the functions:
isCar :: Vehicle -> Bool
-- isCar = undefined
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
-- isPlane = undefined
isPlane (Plane _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
-- areCars = undefined
areCars = map (\x -> isCar x)


-- 3. Now we’re going to write a function to tell us the manufacturer
-- of a piece of data:
getManu :: Vehicle -> Manufacturer
-- getManu = undefined
getManu (Car manu _) = manu


-- 4. Given that we’re returning the Manufacturer, what will happen
-- if you use this on Plane data?
-- Non-exhaustive patterns in function getManu


-- 5. All right. Let’s say you’ve decided to add the size of the plane as an
-- argument to the Plane constructor. Add that to your datatypes
-- in the appropriate places and change your data and functions
-- appropriately.

doge2 = Plane2 PapuAir Big


isPlane2 :: Vehicle2 -> Bool
isPlane2 (Plane2 _ _) = True
isPlane2 _            = False
