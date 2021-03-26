-- without the Num typeclass constraint we would get an error because addition only works on types that are an instance of Num

add :: Num a => a -> a -> a
add x y = x + y

-- x y must be of the same type because addition is done with elements of the same type.
-- a must be an instance of Num and Ord because the types must be added and compared.

addWeird :: (Num a, Ord a) => a -> a -> a
addWeird x y =
    if x > 1
        then x + y
        else x

-- a must implement Eq or Ord typeclass (an Ord typeclass constraint implies an Eq typeclass constraint)

check' :: Ord a => a -> a -> Bool
check' a a' = a == a'

-- It we used concrete types (Int for example) all the above would typecheck !!!

