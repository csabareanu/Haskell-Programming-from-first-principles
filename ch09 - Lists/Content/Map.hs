module Map where

-- map can only be used with [].
-- fmap is defined in a typeclass named Functor and can be applied to
-- data other than lists.

-- A common mantra for performance sensitive code in Haskell is, “lazy in the spine, strict in the leaves.”
