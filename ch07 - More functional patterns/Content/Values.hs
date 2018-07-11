module Values where

addOne :: Integer -> Integer
addOne x = x + 1

z :: Integer
z = addOne 1 -- x is now bound to 1. Binding variables through function application

bindExpLet :: Integer -> String
bindExpLet x = let y = 5 in     -- x visible anywhere in the function
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y -- y in scope because the let expression binds y to 5

bindExp :: Integer -> String
bindExp x = let x = 10; y = 5 in
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y

-- the x value is shadowed by the x from the let binding. The innermost definition of a variable takes precedence over the other ones bc. Haskell is lexically scoped.
