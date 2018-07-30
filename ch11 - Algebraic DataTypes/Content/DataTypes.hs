module DataTypes where

-- PugType is a type constant bc it has no args.
-- PugData is a constant value bc it has no args.
data PugType = PugData

-- HuskyType is a type constructor which takes a single parametrically polymorphic type variable as an argument. This is a phantom bc. it does not occur as an argument anywhere after the ==
-- HuskyData is the data constructor
data HuskyType a = HuskyData

-- doge must equal doge.
data DogueDeBordeaux doge = DogueDeBordeaux doge


myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => a -> HuskyType a
myOtherHusky a = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

--this will not work:
-- badDoge :: DogueDeBordeaux String
-- badDoge = DogueDeBordeaux 10

data Doggies a =
      Husky a
    | Mastiff a
    deriving (Eq, Show)
