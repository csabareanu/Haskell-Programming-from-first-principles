module Identity where


-- -- f ~ Identity
-- -- Applicative f =>
-- (<*>) ::        f (a -> b) ->        f a ->        f b
-- (<*>) :: Identity (a -> b) -> Identity a -> Identity b

-- pure :: a ->        f a
-- pure :: a -> Identity a

-- Having an extra structure around the values lifts some function from mapping over the original structure to mapping over Identity
-- Prelude> const <$> [1, 2, 3] <*> [9, 9, 9]
-- [1,1,1,2,2,2,3,3,3]
--
-- const takes 2 values and we partially apply [1,2,3]
-- [const 1, const 2, const 3] <*> [9,9,9]
-- [const 1 9, const 1 9, const 1 9, const 2 9, const 2 9, const 2 9, const 3 9, const 3 9, const 3 9]
-- [1,1,1,2,2,2,3,3,3]

-- Prelude> const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9]
-- Identity [1,2,3]

-- const (Identity [1,2,3]) <*> (Identity [9,9,9])
--           f     (a->b)             f     a
-- Identity [1,2,3]


-- const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9]
-- Identity [1,2,3]
-- Write an Applicative instance for Identity.

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure                            = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)