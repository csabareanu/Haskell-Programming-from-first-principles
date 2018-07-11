module Anonymous where

--  (\x -> x * 3) :: Integer -> Integer similar to
--  triple x = x * 3

-- It's also possible to:
-- triple = (\x -> x * 3) :: Integer -> Integer

-- To apply an anonymous function:
-- (\x -> x * 3) 3   // 9
-- If the above is not paranthesized the computer will try to apply 1 to 3 as if 3 is a function.
