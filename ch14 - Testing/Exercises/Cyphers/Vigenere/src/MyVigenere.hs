module MyVigenere where

import Data.Char(ord,chr, toLower)

encode :: Char -> Int
encode x = ord x - ord 'a'

decode :: Int -> Char
decode x = chr (x + ord 'a')

shiftRight :: Int -> Char -> Char
shiftRight = shift (+)

shiftLeft :: Int -> Char -> Char
shiftLeft = shift (-)

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift f x y = decode $ mod (f (encode y) x) 26

charsPerWord :: [Char] -> [Int]
charsPerWord = map length . words

mapKeyword :: [Char] -> [Int] -> [Char]
mapKeyword _  []     = []
mapKeyword ks (x:xs) = take x ks ++ " " ++ mapKeyword (drop x ks) xs

vigenere :: [Char] -> [Char] -> [Char]
vigenere k m =
    zipWith (\a b -> if a /= ' ' then (shiftRight (encode a) b) else ' ')
    (mapKeyword (cycle k) . charsPerWord $m)
    m

unvigenere :: [Char] -> [Char] -> [Char]
unvigenere k m =
    zipWith (\a b -> if a /= ' ' then (shiftLeft (encode a) b) else ' ')
    (mapKeyword (cycle k) . charsPerWord $m)
    m