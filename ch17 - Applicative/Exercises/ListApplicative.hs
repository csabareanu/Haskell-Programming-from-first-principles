module ListApplicative where

-- Implement the List Applicative. Writing a minimally complete Applicative
-- instance calls for writing the definitions of both pure and
-- <*>. We’re going to provide a hint as well. Use the checkers library to
-- validate your Applicative instance.
data List a =
        Nil
        | Cons a (List a)
        deriving (Eq, Show)
-- Remember what you wrote for the List Functor:

instance Functor List where
    fmap = undefined

-- Writing the List Applicative is similar.
instance Applicative List where
    pure = undefined
    (<*>) = undefined

-- Expected result:
-- Prelude> let functions = Cons (+1) (Cons (*2) Nil)
-- Prelude> let values = Cons 1 (Cons 2 Nil)
-- Prelude> functions <*> values
-- Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

-- In case you get stuck, use the following functions and hints.
-- append :: List a -> List a -> List a
-- append Nil ys = ys
-- append (Cons x xs) ys = Cons x $ xs `append` ys
-- fold :: (a -> b -> b) -> b -> List a -> b
-- fold _ b Nil = b
-- fold f b (Cons h t) = f h (fold f b t)
-- concat' :: List (List a) -> List a
-- concat' = fold append Nil
-- -- write this one in terms of concat' and fmap
-- flatMap :: (a -> List b) -> List a -> List b
-- flatMap f as = undefined
-- Use the above and try using flatMap and fmap without explicitly
-- pattern-matching on Cons cells. You’ll still need to handle the Nil
-- cases.
-- flatMap is less strange than it would initially seem. It’s basically “fmap,
-- then smush”.
-- Prelude> fmap (\x -> [x, 9]) [1, 2, 3]
-- [[1,9],[2,9],[3,9]]
-- Prelude> flatMap (\x -> [x, 9]) [1, 2, 3]
-- [1,9,2,9,3,9]
-- Applicative instances, unlike Functors, are not guaranteed to have a
-- unique implementation for a given datatype.