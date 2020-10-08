module Bool_interm where

-- The following lines of code may have mistakes — some of them won’t
-- compile! You know what you need to do.

-- 1. not True && true
-- will not compile because true is not a data constructor
-- Correct: not True && True -> False

-- 2. not (x = 6)
-- Will not compile because = is not a compare operator.
-- Another error could be that x is not defined
-- Correct: not (x == 6) -> True if x = 5.

-- 3. (1 * 2) > 5
-- Does compile . -> False

-- 4. [Merry] > [Happy]
-- It could compile if Merry and Happy are data constructors in this exercise and their type constructor implements Ord
-- Correct: "Merry" > "Happy" -> True

-- 5. [1, 2, 3] ++ "look at me!"
-- It will not compile because "look at me!" is not of type [Int]
-- Correct "123" ++ "look at me!"