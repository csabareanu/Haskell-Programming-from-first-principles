module SumTypes_interm where

import Data.Int
-- 1. Given a datatype
data BigSmall =
    Big Bool
    | Small Bool
    deriving (Eq, Show)
-- What is the cardinality of this datatype? Hint: We already know
-- Bool’s cardinality. Show your work as demonstrated earlier.

-- 2 + 2 = 4


-- 2. Given a datatype
-- -- needed to have Int8 in scope

data NumberOrBool =
    Numba Int8
    | BoolyBool Bool
    deriving (Eq, Show)
-- -- Example use of Numba, parentheses due to
-- -- syntactic collision between (-) minus and
-- -- the negate function
-- let myNumba = Numba (-128)
-- What is the cardinality of NumberOrBool? What happens if you
-- try to create a Numba with a numeric literal larger than 127? And
-- with a numeric literal smaller than (-128)?

-- Cardinality : 256 + 2 = 258
-- warning : [-Woverflowed-literals] Literal is out of the Int8 range -128..127


-- If you choose (-128) for a value precisely, you’ll notice you get
-- a spurious warning:
-- Prelude> let n = Numba (-128)
-- Literal 128 is out of the Int8 range -128..127
-- If you are trying to write a large negative
-- literal, use NegativeLiterals
-- Now, since -128 is a perfectly valid Int8 value you could choose to
-- ignore this. What happens is that (-128) desugars into (negate
-- 128). The compiler sees that you expect the type Int8, but
-- Int8’s maxBound is 127. So even though you’re negating 128, it
-- hasn’t done that step yet and immediately whines about 128 being
-- larger than 127. One way to avoid the warning is the following:
-- Prelude> let n = (-128)
-- Prelude> let x = Numba n
-- Or you can use the NegativeLiterals extension as it recommends:
-- Prelude> :set -XNegativeLiterals
-- Prelude> let n = Numba (-128)
-- Note that the negative literals extension doesn’t prevent the warning if you use negate.cd
