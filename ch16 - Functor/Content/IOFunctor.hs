module IOFunctor where

-- getLine :: IO String
-- read :: Read a => String -> a

getInt :: IO Int
getInt = fmap read getLine

-- getLine returns IO String and when we fmap over IO String, this will lift read over the IO type.
-- This also works because int has a Read instance

meTooIsm :: IO String
meTooIsm = do
    input <- getLine
    return (input ++ " and me too!")

bumpIt :: IO Int
bumpIt = do
    intVal <- getInt
    return (intVal + 1)