module Main where

-- Remember the “numbers into words” exercise in Recursion? You’ll
-- be writing tests to validate the functions you wrote.

import Test.Hspec
import WordNumber (digitToWord, digits, wordNumber)

main :: IO ()
main = hspec $ do
        describe "digitToWord does what we want" $ do
            it "returns zero for 0" $ do
                digitToWord 0 `shouldBe` "zero"
            it "returns one for 1" $ do
                digitToWord 1 `shouldBe` "one"
        describe "digits does what we want" $ do
            it "returns [1] for 1" $ do
                digits 1 `shouldBe` [1]
            it "returns [1, 0, 0] for 100" $ do
                digits 100 `shouldBe` [1,0,0]
        describe "wordNumber does what we want" $ do
            it "returns one-zero-zero for 100" $ do
                wordNumber 100 `shouldBe` "one-zero-zero"
            it "returns nine-zero-zero-one for 9001" $ do
                wordNumber 9001 `shouldBe` "nine-zero-zero-one"


-- Fill in the test cases that print question marks. If you think of additional
-- tests you could perform, add them.