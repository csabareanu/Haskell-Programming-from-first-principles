module Spines where

-- is the connective structure that ties the collection of values together.
-- In the case of the list, the spine is usually textually represented by the recursive cons (:) operators.
--   :
--  / \
-- 1   :
--    / \
--   2   :
--      / \
--      3 []

-- Non-Strict Evaluation -> nothing is evaluated until it must be.
-- In Haskell, values get reduced to WEAK HEAD NORMAL FORM by default.
-- NORMAL FORM -> expression fully evaluated
-- WEAK HEAD NORMAL FORM -> expression evaluated as far as is necessary to reach a data constructor.
--      This is a larger set than N.F. and contains both the possibility that the expression is fully evaluated and the possibility that the expression has been evaluated to the point
--          of arriving at a data constructo or lambda awaiting an arg.
-- If no further inputs are possible, the expression is both in WHNF and NF

-- (1, 2) -- WHNF & NF

-- (1, _ + _)
-- -- WHNF, but not NF. The (+) and its
-- -- unknown arguments could be evaluated

-- (1, 1 + 1)
-- -- WHNF, but not NF.
-- -- The 1 + 1 could be evaluated.

-- \x -> x * 10 -- WHNF & NF
-- -- It's in normal form because while
-- -- (*) has been applied to two arguments of a sort
-- -- It cannot be reduced further until the outer \x -> ...
-- -- has been applied.
-- -- With nothing further to reduce it is in normal form.

-- "Papu" ++ "chon" -- Neither WHNF nor NF

-- length function recurses the spine and counts how many cons cells there are. It would force the evaluation of the entire spine without accompanying strictness in the values
