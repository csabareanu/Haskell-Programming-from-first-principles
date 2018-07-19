module Pattern_matching where

myTail          :: [a] -> [a]
myTail []       = []
myTail (_ : xs) = xs

-- rewriting myTail with Maybe datatype
safeTail         :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (x : []) = Nothing
safeTail (_ : xs) = Just xs

-- rewriting myHead with Maybe datatype
myHead        :: [a] -> Maybe a
myHead []      = Nothing
myHead (x : _) = Just x

-- Syntactic sugar:
-- [1, 2, 3] ++ 4 is syntactic sugar for
-- (1 : 2 : 3 : []) ++ 4 : []

-- a : [a] -> the cons cells, the result of recursively prepending a value to more list
-- spine -> connective structure that holds the cons cells together and in place.
