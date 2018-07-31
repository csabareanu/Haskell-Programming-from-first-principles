module DataTypes_unary_interm where

-- 1. Reusing the TooMany typeclass, write an instance of the typeclass
-- for the type (Int, String). This will require adding a language
-- pragma named FlexibleInstances4 if you do not use a newtype —
-- GHC will tell you what to do.


-- 2. Make another TooMany instance for (Int, Int). Sum the values
-- together under the assumption this is a count of goats from two
-- fields.


-- 3. Make another TooMany instance, this time for (Num a, TooMany
-- a) => (a, a). This can mean whatever you want, such as summing
-- the two numbers together.