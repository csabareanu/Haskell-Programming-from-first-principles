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
