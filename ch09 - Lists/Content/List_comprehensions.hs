module List_comprehensions where

-- generating a new list from a list or lists
-- they come from the concept of set comprehensions in mathematics with similar syntax.

--   [ x^2 | x <- [1..10]]
-- -- [1] [2]    [ 3 ]

-- 1. This is the output function that will apply to the members of the
-- list we indicate.
-- 2. The pipe here designates the separation between the output
-- function and the input.
-- 3. This is the input set: a generator list and a variable that represents
-- the elements that will be drawn from that list. This says, “from a
-- list of numbers from 1-10, take (<-) each element as an input to
-- the output function.”

-- If there are more generators the rightmost will be exhausted first.
-- Prelude> [x^y | x <- [1..10], y <- [2, 3], x^y < 200]
-- [1,1,4,8,9,27,16,64,25,125,36,49,64,81,100]