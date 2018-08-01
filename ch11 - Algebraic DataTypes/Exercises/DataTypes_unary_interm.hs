{-# LANGUAGE FlexibleInstances #-}

module DataTypes_unary_interm where

-- 1. Reusing the TooMany typeclass, write an instance of the typeclass
-- for the type (Int, String). This will require adding a language
-- pragma named FlexibleInstances4 if you do not use a newtype —
-- GHC will tell you what to do.

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany (Int, String) where
    tooMany (x, _) = tooMany x


-- 2. Make another TooMany instance for (Int, Int). Sum the values
-- together under the assumption this is a count of goats from two
-- fields.

instance TooMany (Int, Int) where
    tooMany (x, y) = tooMany (x + y)


-- 3. Make another TooMany instance, this time for (Num a, TooMany
-- a) => (a, a). This can mean whatever you want, such as summing
-- the two numbers together.

newtype Tuple a = Tuple (a, a)


instance (Num a, TooMany a) => TooMany (Tuple a) where
    tooMany (Tuple (x, y)) = tooMany (x + y)