module Case_expr where

funcA :: (Num a, Eq a) => a -> [Char]
funcA x = if x + 1 == 1 then "Zero" else "Another Value"

funcB :: (Num a, Eq a) => a -> [Char]
funcB x =
    case x + 1 == 1 of
        True -> "Zero"
        False -> "Another Value"

pal :: Eq a => [a] -> [Char]
pal x =
    case x == reverse x of
        True -> "Yes"
        False -> "No"

-- can be rewritten:
pal' :: Eq a => [a] -> [Char]
pal' x =
    case y of
        True -> "Yes"
        False -> "No"
    where
        y = x == reverse x
