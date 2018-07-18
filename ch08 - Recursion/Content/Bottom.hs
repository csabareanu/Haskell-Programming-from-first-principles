module Bottom where

-- Is used to refer to computations that do not succesfully result in a value. There are 2 types:
-- a) Computations that failed with an error
-- b) Computations that failed to terminate

-- In logic, it coresponds to false

f :: Bool -> Int
f True = error "blah" -- this too is Bottom
f False = 0

g :: Bool -> Int
g False = 0 -- this is also an example of Bottom because it is a partial function(does not handle all of its inputs) (Non-exhaustive patterns in function g)

-- g is really equivalent with :
-- g :: Bool -> Int
-- g False = 0
-- g _ = error $ "*** Exception: "
--             ++ "Non-exhaustive"
--             ++ "patterns in function f"


--data Maybe a = Nothing | Just a

h :: Bool -> Maybe Int
h False = Just 0     -- the value is wrapped in the Just constructor
h _     = Nothing    -- If we want to say that there is no result or data from the function without hitting Bottom
