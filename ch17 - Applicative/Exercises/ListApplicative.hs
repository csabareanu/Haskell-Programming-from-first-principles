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
    fmap _ Nil              = Nil
    fmap f (Cons head tail) = Cons (f head) (fmap f tail)

-- Writing the List Applicative is similar.
-- a naive version
instance Applicative List where
    pure f                             = Cons f Nil
    (<*>) _           Nil              = Nil
    (<*>) Nil         _                = Nil
    (<*>) (Cons f fs) (Cons head tail) = pure (f head) `append` (fmap f tail) `append` ((<*>) fs (pure head)) `append` ((<*>) fs tail)

-- Expected result:
-- Prelude> let functions = Cons (+1) (Cons (*2) Nil)
-- Prelude> let values = Cons 1 (Cons 2 Nil)
-- Prelude> functions <*> values
-- Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

-- In case you get stuck, use the following functions and hints.
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

-- f ~ append , b ~ Nil
fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)


-- List (List a) is:
-- Cons 5 (Cons 4 (Cons 3 Nil)) -> [5,4,3] :: Num a => List a
-- Cons 5 (Cons (Cons 4 (Cons 2 Nil)) (Cons 1 Nil)) :: (Num a, Num (List a)) => List (List a)

concat' :: List (List a) -> List a
concat' = fold append Nil
-- -- write this one in terms of concat' and fmap


-- *ListApplicative> flatMap (\x -> Cons x (Cons 9 Nil)) (Cons 1 (Cons 2 (Cons 3 Nil)))
-- Cons 1 (Cons 9 (Cons 2 (Cons 9 (Cons 3 (Cons 9 Nil)))))
-- *ListApplicative> fmap (\x -> Cons x (Cons 9 Nil)) (Cons 1 (Cons 2 (Cons 3 Nil)))
-- Cons (Cons 1 (Cons 9 Nil)) (Cons (Cons 2 (Cons 9 Nil)) (Cons (Cons 3 (Cons 9 Nil)) Nil))

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f
-- *ListApplicative> fmap (\x -> Cons x (Cons 9 Nil)) (Cons 1 (Cons 2 (Cons 3 Nil)))
-- Cons (Cons 1 (Cons 9 Nil)) (Cons (Cons 2 (Cons 9 Nil)) (Cons (Cons 3 (Cons 9 Nil)) Nil))
-- concat' ( Cons (Cons 1 (Cons 9 Nil)) (Cons (Cons 2 (Cons 9 Nil)) (Cons (Cons 3 (Cons 9 Nil)) Nil)) ) ( -> List a )
-- fold append Nil ( Cons (Cons 1 (Cons 9 Nil)) (Cons (Cons 2 (Cons 9 Nil)) (Cons (Cons 3 (Cons 9 Nil)) Nil)) )
--                   Cons       HEAD                                    TAIL
-- append (Cons 1 (Cons 9 Nil)) (fold append Nil (Cons (Cons 2 (Cons 9 Nil)) (Cons (Cons 3 (Cons 9 Nil)) Nil)) )
--                                                Cons        HEAD                       TAIL
-- append (Cons 1 (Cons 9 Nil)) (append (Cons 2 (Cons 9 Nil)) (fold append Nil (Cons (Cons 3 (Cons 9 Nil)) Nil)) )
--                                                                              Cons        HEAD           TAIL
-- append (Cons 1 (Cons 9 Nil)) (append (Cons 2 (Cons 9 Nil)) (append (Cons 3 (Cons 9 Nil)) fold append Nil Nil )))
 --append (Cons 1 (Cons 9 Nil)) (append (Cons 2 (Cons 9 Nil)) (append (Cons 3 (Cons 9 Nil)) Nil))
 --                                                                         x       xs
 -- append (Cons 1 (COns 9 Nil)) (append (Cons 2 (Cons 9 Nil)) (Cons 3 $ (Cons 9 Nil) `append` Nil))
 --                                                                            x  xs
--  append (Cons 1 (Cons 9 Nil)) (append (Cons 2 (Cons 9 Nil)) (Cons 3 $ Cons 9 $ Nil `append` Nil)
--                                                                                     Nil
-- append (Cons 1 (Cons 9 Nil)) (append (Cons 2 (Cons 9 Nil)) (Cons 3 $ Cons 9 Nil)
-- append (Cons 1 (Cons 9 Nil)) (append (Cons 2 (Cons 9 Nil)) (Cons 3 (Cons 9 Nil))
-------
-- append (Cons 2 (Cons 9 Nil)) (Cons 3 (Cons 9 Nil)
--              x      xs                ys
-- Cons 2 $ (Cons 9 Nil) `append` (Cons 3 (Cons 9 Nil))
--                       x  xs                  ys
-- Cons 2 $ Cons 9 $ Nil `append` (Cons 3 (Cons 9 Nil))
-- Cons 2 $ Cons 9 $ (Cons 3 (Cons 9 Nil))
-- Cons 2 (Cons 9 (Cons 3 (Cons 9 Nil)))

--- etc etc etc.

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