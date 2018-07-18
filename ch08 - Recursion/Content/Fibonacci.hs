module Fibonacci where


-- Definition:
-- each number is the sum of the previous two numbers
-- 1 1 2 3 5 8 13 21 34 55 89

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

-- fibonacci 5
-- fibonacci 5 = fibonacci 4                                                         + fibonacci 3
-- fibonacci 5 = fibonacci 3                             + fibonacci 2               + fibonacci 2               + fibonacci 1
-- fibonacci 5 = fibonacci 2               + fibonacci 1 + fibonacci 1 + fibonacci 0 + fibonacci 1 + fibonacci 0 + 1
-- fibonacci 5 = fibonacci 1 + fibonacci 0 + 1           + 1           + 0           + 1           + 0           + 1
-- fibonacci 5 = 1           + 0           + 4
-- fibonacci 5 = 5