module Filter where

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter _ [] = []
-- filter pred (x:xs)
----  | pred x = x : filter pred xs
----  | otherwise = filter pred xs

-- Prelude> filter (\x -> elem x "aeiou") "abracadabra"
-- "aaaaa"
-- Prelude> [x | x <- "abracadabra", elem x "aeiou"]
-- "aaaaa"