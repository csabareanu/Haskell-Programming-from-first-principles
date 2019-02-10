module Tests where

import Test.Hspec
import Test.QuickCheck
import MyVigenere

keyword :: String
keyword = "Hello"

-- main :: IO ()
-- main = hspec $ do
--             describe "Vigenere" $ do
--                 it "1 + 1 is greater than 1" $ do
--                     (1+1)>(1::Int) `shouldBe` True
--                 it "Decode applied after Encode should be the same as the original input" $ do
--                     property $ \x -> x == (unvigenere keyword . vigenere keyword) (x :: String)

testVigenereIdentity :: IO ()
testVigenereIdentity = quickCheck (\x -> x == (unvigenere keyword . vigenere keyword) x )