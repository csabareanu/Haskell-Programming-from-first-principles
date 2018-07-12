module Guards where

-- Guard syntax allows us to write compact functions that allow for two or more possible outcomes depending on the truth of the conditions.

myAbs :: Integer -> Integer
myAbs x
    | x < 0     = (-x)
    | otherwise = x   -- otherwise == True. Used as a fallback if the first condition was False

-- | pipe -> guard case
-- Guards evaluate sequentially and the guards should be ordered from the most restrictive case to the least restrictive.

bloodNa :: Integer -> String
bloodNa x
    | x < 135   = "Too low"
    | x > 145   = "Too high"
    | otherwise = "Just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
        | a^2 + b^2 == c^2 = "Right On"
        | otherwise        = "Not right"

-- where expressions can be used with Guards
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.9  = 'A'
    | y >= 0.8  = 'B'
    | y >= 0.7  = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59  = 'F'
    where y = x / 100
