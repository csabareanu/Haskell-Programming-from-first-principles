module WordNumber where

import Data.List (intercalate)

wordNumber :: Int -> String
wordNumber 0 = digitToWord 0
wordNumber n = intercalate "-" $ map digitToWord $ digits n

digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [(n `mod` 10)]

digitToWord :: Int -> String
digitToWord n
    | n == 0         = "zero"
    | n == 1         = "one"
    | n == 2         = "two"
    | n == 3         = "three"
    | n == 4         = "four"
    | n == 5         = "five"
    | n == 6         = "six"
    | n == 7         = "seven"
    | n == 8         = "eight"
    | n == 9         = "nine"
    | otherwise = "NaN"
