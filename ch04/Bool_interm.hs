module Bool_interm where

-- The following lines of code may have mistakes — some of them won’t
-- compile! You know what you need to do.
-- 1. not True && true

-- Does not compile.
-- Correct : not True && True


-- 2. not (x = 6)

-- Does not compile. x not in scope, x = 6 does not evaluate to Bool
-- Correct : not (x == 6) // if x in scope


-- 3. (1 * 2) > 5

-- Does compile.


-- 4. [Merry] > [Happy]

-- Does not compile.
-- Correct: "Merry" > "Happy"


-- 5. [1, 2, 3] ++ "look at me!"

-- Does not compile. Not the same type, even if (++) accepts both types.
-- Correct: [1,2,3] ++ [5,6]   |  "You should " ++ "look at me!"