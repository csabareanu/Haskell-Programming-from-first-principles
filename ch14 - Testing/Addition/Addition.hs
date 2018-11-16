module Addition where

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4,2)

    describe "Multiply" $ do
        it "3 * 5 is 15" $ do
            multiply 3 5 `shouldBe` 15
        it "5 * 1 is 5" $ do
            multiply 5 1 `shouldBe` 5
        it "4 * 0 is 0" $ do
            multiply 4 0 `shouldBe` 0





dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d     = (count, n)
            | otherwise = go (n - d) d (count + 1)

multiply :: (Eq a, Num a) => a -> a -> a
multiply _ 0 = 0
multiply n 1 = n
multiply n m = n + multiply n (m - 1)
