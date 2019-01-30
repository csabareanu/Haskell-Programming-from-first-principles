module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d     = (count, n)
            | otherwise = go (n - d) d (count + 1)

multiplyBy :: (Eq a, Num a) => a -> a -> a
multiplyBy a b
            | b == 0    = 0
            | otherwise = a + multiplyBy a (b - 1)

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
        -- with QuickCheck
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

    describe "Division" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5,0)
        it "22 divided by 5 is 4, remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4,2)
    describe "Multiply" $ do
        it "7 multiplied by 6 is 42" $ do
            multiplyBy 7 6 `shouldBe` 42
        it "6 multiplied by 0 is 0" $ do
            multiplyBy 6 0 `shouldBe` 0

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

genBool :: Gen Bool
genBool = choose (True, False)

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a,b)