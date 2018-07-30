module Cardinality where


-- Cardinality : the number of possible values a datatype can define.


-- While we haven’t explicitly described the rules for calculating the
-- cardinality of datatypes yet, you might already have an idea of how
-- to do it for simple datatypes with nullary constructors. Try not to
-- overthink these exercises – you can probably intuitively grasp what
-- the cardinality is based just on what you know.

-- 1.data PugType = PugData
-- 1

-- 2. For this one, recall that Bool is also defined with the | :
-- data Airline =
-- PapuAir
---- | CatapultsR'Us
---- | TakeYourChancesUnited
-- 3


-- 3. Given what we know about Int8 , what’s the cardinality of Int16 ?
-- Prelude Data.Int> maxBound :: Int16
-- 32767
-- Prelude Data.Int> minBound :: Int16
-- -32768
-- = 32767 + 32768 + 1 = 65.536


-- 4. Use the REPL and maxBound and minBound to examine Int and
-- Integer . What can you say about the cardinality of those types?
-- Prelude Data.Int> minBound :: Int
-- -9223372036854775808
-- Prelude Data.Int> maxBound :: Int
-- 9223372036854775807
-- The cardinality of `Int` is 18446744073709551616
-- Cardinality of Integer is Infinite


-- 5. Extra credit (impress your friends!): What’s the connection be-
-- tween the 8 in Int8 and that type’s cardinality of 256?
-- the 8 means 8 bits. The number of values that could be represented using 8 bits is 2^8
