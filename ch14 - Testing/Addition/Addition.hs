module Addition where

import Test.Hspec
import Test.QuickCheck


-- for QuickCheck Testing
-- *Addition> :t sample'
-- sample' :: Gen a -> IO [a]
-- *Addition> sample' trivialInt
-- [1,1,1,1,1,1,1,1,1,1,1]
trivialInt :: Gen Int
trivialInt = return 1

-- *Addition> :t elements
-- elements :: [a] -> Gen a
-- *Addition> sample' oneThroughThree
-- [2,3,3,1,3,1,3,3,1,2,2]

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

-- *Addition> :t choose
-- choose :: random-1.1:System.Random.Random a => (a, a) -> Gen a
-- *Addition> sample' genBool
-- [True,True,False,False,False,True,False,False,True,False,False]
-- *Addition> sample' genBool'
-- [True,True,True,False,False,True,True,True,False,True,False]
-- *Addition> sample' genChar
-- "yhvqtkylklu"
-- *Addition> sample' genOrdering
-- [GT,EQ,GT,LT,GT,GT,EQ,LT,LT,EQ,EQ]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [True, False]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']


-- Prelude> sample (genTuple :: Gen (Int, Float))
genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary -- binds to a the value x in Gen x
    b <- arbitrary
    return (a,b)   -- returns (a, b) inside the monad Gen

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a,b,c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)


-- *Addition> sample' (genEither :: Gen (Either Int Float))
-- [Right 0.0,Left (-1),Left 0,Left 3,Left 6,Right 9.881076,Left 8,Right 10.477276,Right (-8.178714),Left (-1),Left 12]
genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

-- using QuickCheck without hspec
-- Prelude> runQc
-- +++ OK, passed 100 tests.

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQC :: IO ()
runQC = quickCheck prop_additionGreater


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
        --QuickCheck
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

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
