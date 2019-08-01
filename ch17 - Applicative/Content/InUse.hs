module InUse where

-- List Applicative

-- (<*>) :: f  (a -> b) -> f  a -> f  b
-- (<*>) :: [] (a -> b) -> [] a -> [] b
-- (<*>) :: [(a -> b)]  -> [a]  -> [b]

-- pure :: a -> f  a
-- pure :: a -> [] a

-- With the List functor, we are mapping a SINGLE function over MULTIPLE values
-- With the List Applicative, we are mapping MULTIPLE functions over MULTIPLE values

-- Prelude> (,) <$> [1, 2] <*> [3, 4]
-- [(1,3),(1,4),(2,3),(2,4)]

-- -- first we fmap the (,) over the first list
-- [(1, ), (2, )] <*> [3, 4]
-- -- then we apply the first list
-- -- to the second
-- [(1,3),(1,4),(2,3),(2,4)]


-- Prelude> (+) <$> [1, 2] <*> [3, 5]
-- [4,6,5,7]
-- Prelude> liftA2 (+) [1, 2] [3, 5]  -- in Control.Applicative lib
-- [4,6,5,7]


import Control.Applicative

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]