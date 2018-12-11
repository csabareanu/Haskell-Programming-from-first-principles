module Main where

import MyHangman
import Test.Hspec
import Test.Hspec.QuickCheck (prop)


-- prop :: (HasCallStack, Testable prop) => String -> prop -> Spec
-- prop ".." $ .. is a shortcut for
-- it ".." $ do property $ ..

-- forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
main :: IO ()
main = hspec $ do
    describe "fillInCharacter" $ do
        prop "always adds the character to the already guessed list" $
            forAll puzzleWithAnyCharGen $
                \(puzzle, ch) ->
                    let guessed = guessedChars puzzle
                    in guessedChars (fillInCharacter puzzle ch) == ch : guessed


-- Puzzle and char that is contained in the word
puzzleWithExistingCharGen :: Gen (Puzzle, Char)
puzzleWithExistingCharGen = do
        puzzle <- puzzleGen False
        char   <- elements (word puzzle)
        return (puzzle, char)

puzzleWithAnyCharGen :: Gen (Puzzle, Char)
puzzleWithAnyCharGen = oneof [ puzzleWithNonExistingCharGen
                                , puzzleWithExistingCharGen ]

puzzleGen :: Gen Puzzle
puzzleGen = do
    word' <-wordGen 5 9


wordGen :: Int -> Int -> Gen String
wordGen mn mx = do
    k <- choose (mn, mx) :: Gen Int
    sequence [ letterGen | _ <- [1..k] ]

letterGen :: Gen Char
letterGen = elements letterRange

letterRange :: [Char]
letterRange = ['a'..'z']