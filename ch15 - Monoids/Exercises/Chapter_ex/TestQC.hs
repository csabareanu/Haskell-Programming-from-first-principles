import Data.Monoid
import Control.Monad
import Test.QuickCheck

type S = String
type B = Bool


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- *Main> quickCheck (monoidAssoc :: S->S->S->B)
-- +++ OK, passed 100 tests.

monoidLeftId :: (Eq m, Monoid m) => m -> Bool
monoidLeftId a = (mempty <> a) == a

monoidRightId :: (Eq m, Monoid m) => m -> Bool
monoidRightId a = (a <> mempty) == a

-- *Main> quickCheck (monoidLeftId :: S->B)
-- +++ OK, passed 100 tests.
-- *Main> quickCheck (monoidRightId :: S->B)
-- +++ OK, passed 100 tests.

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary =
        frequency [ (1, return Fools)
                  , (1, return Twoo) ]

instance Semigroup Bull where
    (<>) _ _ = Fools

instance Monoid Bull where
    mempty      = Fools
    mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: BullMappend)
    quickCheck (monoidLeftId :: Bull -> Bool)
    quickCheck (monoidRightId :: Bull -> Bool)

-- *Main> main
-- +++ OK, passed 100 tests.
-- *** Failed! Falsifiable (after 1 test):
-- Twoo
-- *** Failed! Falsifiable (after 2 tests):
-- Twoo

-- Fails Left and Right Identity because:
-- Should:
-- mappend mempty x = x
-- Is:
-- mappend mempty x = Fools (is not x)