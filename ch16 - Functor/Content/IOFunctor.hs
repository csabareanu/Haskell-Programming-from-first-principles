module IOFunctor where

-- getLine :: IO String
-- read :: Read a => String -> a

getInt :: IO Int
getInt = fmap read getLine

-- getLine returns IO String and when we fmap over IO String, this will lift read over the IO type.
-- This also works because int has a Read instance

meTooIsm :: IO String
meTooIsm = do
    input <- getLine  -- input is String type
    return (input ++ " and me too!") -- wraps String in IO String type

bumpIt :: IO Int
bumpIt = do
    intVal <- getInt -- intVal is Int type
    return (intVal + 1) -- wraps Int in IO Int type

-- it is more shorter and clearer to apply fmap for the 2 above functions
