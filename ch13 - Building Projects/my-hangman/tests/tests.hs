module Main where

import Test.QuickCheck
import

main :: IO ()
main = quickCheck (prop_test :: Int -> Bool )

prop_test :: Int -> Bool
prop_test x = x == x

-- puzzleGen :: Gen Puzzle
-- puzzleGen

fill_in_char_prop :: Puzzle -> Char -> Bool
fill_in_char_prop (Puzzle word filledInSoFar s) c = True

fill_in_char_test :: IO ()
fill_in_char_test = quickCheck fill_in_char_prop