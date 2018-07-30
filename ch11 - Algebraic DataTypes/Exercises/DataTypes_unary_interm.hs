module DataTypes_unary_interm where

-- a unary type constructor takes one type argument (not a value)
-- the cardinality is like the type they contain

-- data Goats = Goats Int deriving (Eq, Show)

--for cardinality unary constructors are the identity function.

-----------
-- newtype
-----------

newtype Goats =
    Goats Int deriving (Eq, Show)

newtype Cows =
    Cows Int deriving (Eq, Show)

-- Now we can rewrite our type to be
-- safer, pattern matching in order
-- to access the Int inside our data
-- constructor Goats.

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42
