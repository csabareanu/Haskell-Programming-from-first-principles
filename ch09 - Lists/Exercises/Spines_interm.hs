module Spines_interm where


---------------------
-- Will it blow up?
---------------------

-- 1. Will the following expression return a value or be ⊥?
-- [x^y | x <- [1..5], y <- [2, undefined]]

-- Will blow up


-- 2. take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]

-- returns 1


-- 3. Will the following expression return a value?
-- sum [1, undefined, 3]

-- Will Blow up


-- 4. length [1, 2, undefined]

-- returns 3


-- 5. length $ [1, 2, 3] ++ undefined

-- Will Blow up


-- 6. take 1 $ filter even [1, 2, 3, undefined]

-- returns 2


-- 7. take 1 $ filter even [1, 3, undefined]

--  Will Blow up


-- 8. take 1 $ filter odd [1, 3, undefined]

-- returns 1


-- 9. take 2 $ filter odd [1, 3, undefined]

-- returns [1,3]


-- 10. take 3 $ filter odd [1, 3, undefined]
-- Will Blow Up


-------------------------
-- Is it in normal form?
-------------------------

-- For each expression below, determine whether it’s in:
-- 1. normal form, which implies weak head normal form;
-- 2. weak head normal form only; or,
-- 3. neither.

-- Remember that an expression cannot be in normal form or weak
-- head normal form if the outermost part of the expression isn’t a data
-- constructor. It can’t be in normal form if any part of the expression is
-- unevaluated.

-- 1. [1, 2, 3, 4, 5]
-- NF -> values fully evaluated

-- 2. 1 : 2 : 3 : 4 : _
-- WHNF -> _ not being evaluated and outermost term is (:)

-- 3. enumFromTo 1 10
-- Neither -> Is not a data constructor, enumFromTo has not been applied

-- 4. length [1, 2, 3, 4, 5]
-- Neither -> Length is not a data constructor

-- 5. sum (enumFromTo 1 10)
-- Neither -> sum is not a data constructor


-- 6. ['a'..'m'] ++ ['n'..'z']
-- Neither -> ++ is not a data constructor

-- 7. (_, 'b')
-- WHNF -> _ is not evaluated