module Comprehensions_interm where

-- Take a look at the following functions, figure what you think the output
-- lists will be, and then run them in your REPL to verify (note that you
-- will need the mySqr list from above in scope to do this):

-- [x | x <- mySqr, rem x 2 == 0]
-- [4, 16]


-- [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- []


-- take 5 [ (x, y) | x <- mySqr
--        , y <- mySqr, x < 50, y > 50 ]
-- []

-- What do you think this function would do:
-- Prelude> let myString xs = [x | x <- xs, elem x "aeiou"]
-- gets the vocals from the string xs

-- Given the following:
-- Prelude> let mySqr = [x^2 | x <- [1..5]]
-- Prelude> let myCube = [y^3 | y <- [1..5]]

-- 1. First write an expression that will make tuples of the outputs of
-- mySqr and myCube.
-- [(x, y) | x <- mySqr, y <- myCube]

-- 2. Now alter that function so that it only uses the x and y values
-- that are less than 50.
-- [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3. Now apply another function to that list comprehension to determine
-- how many tuples inhabit your output list.
-- length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]  // 15