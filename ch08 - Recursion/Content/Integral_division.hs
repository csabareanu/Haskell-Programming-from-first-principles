module Integral_division where

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where 
        go :: Integral a => a -> a -> a -> (a, a)
        go n d count 
            | n < d     = (count, n)
            | otherwise = go (n - d) d (count + 1)

-- Here we used a common Haskell idiom called a go function. This
-- allows us to define a function via a where-clause that can accept more
-- arguments than the top-level function dividedBy does.