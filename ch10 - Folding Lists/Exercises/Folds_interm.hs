module Folds_interm where

-- 1. foldr (*) 1 [1..5]            //120
-- will return the same result as which of the following:
-- a) flip (*) 1 [1..5]             //error
-- b) foldl (flip (*)) 1 [1..5]     //120
-- c) foldl (*) 1 [1..5]            //120

-- 1 b) c)


-- 2. Write out the evaluation steps for
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f acc [] = acc
-- foldl f acc (x:xs) = foldl f (f acc x) xs

-- foldl (flip (*)) 1 [1..3]
-- foldl (flip (*)) (1 * 1) [2,3]
-- foldl (flip (*)) (2 * (1 * 1)) [3]
-- foldl (flip (*)) (3 * (2 * (1 * 1))) []
-- (3 * (2 * (1 * 1)))
-- (3 * (2 * 1))
-- (3 * 2)
-- 6


-- 3. One difference between foldr and foldl is:
-- a) foldr, but not foldl, traverses the spine of a list from right
-- to left  // both functions traverse the spine in the same direction. The difference is the associative of the evaluation
-- b) foldr, but not foldl, always forces the rest of the fold // not true. If the function does not evaluate the second argument only f x is evaluated
-- c) foldr, but not foldl, associates to the right
-- d) foldr, but not foldl, is recursive // both are recursive

-- 3 c)


-- 4. Folds are catamorphisms, which means they are generally used to
-- a) reduce structure
-- b) expand structure
-- c) render you catatonic
-- d) generate infinite data structures

-- 4 a)


-- 5. The following are simple folds very similar to what you’ve already
-- seen, but each has at least one error. Please fix them and
-- test in your REPL:

-- a) foldr (++) ["woot", "WOOT", "woot"]
-- does not have an accumulator. For strings this is []
-- foldr (++) [] ["woot", "WOOT", "woot"]

-- b) foldr max [] "fear is the little death"
-- wrong accumulator value. This is not an id element. The right one is '\0' -> null terminated string
-- foldr max '\0' "fear is the little death"

-- c) foldr and True [False, True]
-- and [False, True]
-- // or:
-- foldr (&&) True [False, True]
-- (&&) False (foldr (&&) True [True])
-- (&&) False ((&&) True (foldr (&&) True []))
-- (&&) False ((&&) True True)
-- (&&) False True
-- False
-- Prelude> scanr (&&) True [False,True]
-- [False,True,True]

-- d) This one is more subtle than the previous. Can it ever return
-- a different answer?
-- foldr (||) True [False, True]
-- It should return True all the time because the acc is True and True || True = True , True || False = True
-- We need to modify the accumulator to False
-- foldr (||) False [False, True]

-- e) foldl ((++) . show) "" [1..5]
--  ((++) . show ) first arg must be an int and the second one a Char but we have the other way around.
-- foldl (flip ((++) . show)) "" [1..5]

-- f) foldr const 'a' [1..5]
-- (a -> b -> b) -> b -> [a] -> b
-- does not typecheck. The function does not return an element of type b (Char).
-- foldr const 'a' [1..3]
-- (const (1) (foldr const 'a' [2,3]))
-- (const 1 (const 2 (foldr const 'a' [3])))
-- (const 1 (const 2 (const 3 (foldr const 'a' []))))
-- (const 1 (const 2 (const 3 'a')))
-- (const 1 (const 2 3))
-- (const 1 2)
-- 1
-- foldr const 0 [1..5]

-- g) foldr const 0 "tacos"
-- foldr const 'a' "tacos"

-- h) foldl (flip const) 0 "burritos"
-- foldl (flip const) 'a' "burritos"

-- i) foldl (flip const) 'z' [1..5]
-- foldl (flip const) 0 [1..5]