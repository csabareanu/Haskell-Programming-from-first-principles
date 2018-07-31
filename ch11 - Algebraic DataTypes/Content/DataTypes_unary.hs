-- using language pragma
-- are also called extensions and they are special instructions to the compiler.
-- tells the compiler to process input in other ways the standard does.
-- this language pragma tells the compiler to allow our newtype to rely on a typeclass instance for the type it contains

{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module DataTypes_unary where

-- a unary type constructor takes one type argument (not a value)
-- the cardinality is like the type they contain

-- data Goats = Goats Int deriving (Eq, Show)

--for cardinality unary constructors are the identity function.

-----------
-- newtype
-----------
-- used to mark types constructed with unary data constructors.
-- cannot be a product name, sum type, or contain nullary constructors
-- it has no runtime overhead - it uses the representation of the type it contains.

-- Similar to a type synonym in that the representations of the named type and the type it contains are identical and
-- any distinction between them is gone at compile time. Goats is realy an Int at compile time.

-- One difference is that you can define typeclass instances for newtypes that differ from the instances of their underlying type.

newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)

newtype Cows =
    Cows Int deriving (Eq, Show)

-- Now we can rewrite our type to be
-- safer, pattern matching in order
-- to access the Int inside our data
-- constructor Goats.

-- tooManyGoats :: Goats -> Bool
-- tooManyGoats (Goats n) = n > 4

-- instance TooMany Goats where
--     tooMany (Goats n) = n > 43

-- instance TooMany Goats where
--     tooMany (Goats n) = tooMany n


class TooMany a where
    tooMany :: a -> Bool


instance TooMany Int where
    tooMany n = n > 42
