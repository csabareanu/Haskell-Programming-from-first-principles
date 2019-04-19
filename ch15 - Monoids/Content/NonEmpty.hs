module NonEmpty where

-- data NonEmpty a = a :| [a]
--     deriving (Eq, Ord, Show)

-- the data constructors that begin with : and have no alpha-numeric characters are infix by default. The other ones are prefix by default.

-- import Data.List.NonEmpty as N

-- Prelude N S> let xs = 1 :| [2, 3]
-- Prelude N S> let ys = 4 :| [5, 6]
-- Prelude N S> xs <> ys
-- 1 :| [2,3,4,5,6]
-- Prelude N S> N.head xs
-- 1
-- Prelude N S> N.length (xs <> ys)
-- 6

-- NonEmpty is used as you use just you would a list, but we are explicit that having zero values is not valid for the use-case.