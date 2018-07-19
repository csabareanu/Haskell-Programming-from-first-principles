module List where

-- Double role in Haskell :
-- a) refer and process a collection or plurality of values
-- b) infinite series of values which act as a stream datatype

-- Definition:
-- data [] a = [] | a : [a]
-- (:) -> infix operator called cons which evaluates to [a]. Is a product bc. it takes 2 args.

-- data []  a  =   []  | a : [a]
-- --   [1][2][3] [4] [5] [6]

-- 1. The datatype with the type constructor []
-- 2. takes a single type constructor argument ‘a’
-- 3. at the term level can be constructed via
-- 4. nullary constructor []
-- 5. or it can be constructed by
-- 6. data constructor (:) which is a product of a value of the type a
-- we mentioned in the type constructor and a value of type [a],
-- that is, “more list.”