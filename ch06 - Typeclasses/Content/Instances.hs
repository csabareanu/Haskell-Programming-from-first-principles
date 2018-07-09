module Instances where

-- This is even worse than the last one.
-- Don't use typeclasses to define default values.
-- Seriously. Haskell Ninjas will find you
-- and replace your toothpaste with muddy chalk.

class Numberish a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer
    defaultNumber :: a

newtype Age =
    Age Integer deriving (Eq, Show)

instance Numberish Age where
    fromNumber n = Age n
    toNumber (Age n) = n
    defaultNumber = Age 65

newtype Year =
    Year Integer deriving (Eq, Show)

instance Numberish Year where
    fromNumber n = Year n
    toNumber (Year n) = n
    defaultNumber = Year 1988


sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
    where integerOfA = toNumber a
          integerOfA' = toNumber a'
          summed = integerOfA + integerOfA'
