module Ord_interm where

-- Next, take a look at the following code examples and try to decide
-- if they will work, what result they will return if they do, and why or
-- why not (be sure, as always, to test them in your REPL once you have
-- decided on your answer):


-- 1. max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])

-- Yes. It will work. Length returns an Int, both arguments applied to max are of the same type and have instances of Ord


-- 2. compare (3 * 4) (3 * 5)

-- Yes. It will work. Both arguments are of the same type and implement an instance of Ord


-- 3. compare "Julie" True

-- Does not work. The arguments are of different types.


-- 4. (5 + 3) > (3 + 6)

-- Yes. It will work. Both arguments are of the same type and implement an instance of Ord
