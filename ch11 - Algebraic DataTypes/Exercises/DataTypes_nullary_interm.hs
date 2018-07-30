module DataTypes_nullary_interm where

data Example = MakeExample deriving Show

-- 1. You can query the type of a value in GHCi with the :type com-
-- mand, also abbreviated :t . Example:
-- Prelude> :t False
-- False :: Bool
-- What is the type of data constructor MakeExample ? What hap-
-- pens when you request the type of Example ?

-- MakeExample :: Example
-- It throws an error : Data constructor not in scope


-- 2. What if you try :info on Example in GHCi? Can you determine
-- what typeclass instances are defined for the Example type using
-- :info in GHCi?
-- Yes.
-- *DataTypes_nullary_interm Data.Int> :i Example
-- data Example = MakeExample
--         -- Defined at DataTypes_nullary_interm.hs:3:1
-- instance [safe] Show Example
--   -- Defined at DataTypes_nullary_interm.hs:3:37


-- 3. Try making a new datatype like Example but with a single type
-- argument added to MakeExample , such as Int . What has changed
-- when you query MakeExample with :type in GHCi?

data MyExample = MyMakeExample Int
-- *DataTypes_nullary_interm Data.Int> :t MyMakeExample
-- MyMakeExample :: Int -> MyExample
-- it now needs one argument of type Int to construct a value of type MyExample