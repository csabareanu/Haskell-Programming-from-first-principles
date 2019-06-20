-- language extension used to enable explicit forall sintax
{-# LANGUAGE RankNTypes #-}

module Natural where

-- NATURAL TRANSFORMATIONS are used when the structure needs to be transformed but the type arg (or type constructor) to that structure needs to be left alone.
-- Opposite of what a Functor does.

-------------------------------------
-- nat :: (f -> g) -> f a -> g a
-------------------------------------
-- This type is impossible bc. we can't have higher-kinded types as arg types to the function type. f and g are higher kinded types (later in the type signature, they are taking arguments)
-- fmap :: Functor f => (a -> b) -> f a -> f b



-- the quantification of a in the right-hand side of the declaration allows us to obligate all functions of this type to be oblivious to the contents of the structures f and g.
type Nat f g = forall a . f a -> g a

-- This will work
maybeToList :: Nat Maybe []
maybeToList Nothing  = []
maybeToList (Just a) = [a]

-- This will not work, not allowed.
-- degenerateMtl :: Nat Maybe []
-- degenerateMtl Nothing = []
-- degenerateMtl (Just a) = [a+1]


-- WITHOUT QUANTIFICATION
type Nat2 f g a = f a -> g a

-- This will work
maybeToList2 :: Nat2 Maybe [] a
maybeToList2 Nothing  = []
maybeToList2 (Just a) = [a]

degenerateMtl2 :: Num a => Nat2 Maybe [] a
degenerateMtl2 Nothing  = []
degenerateMtl2 (Just x) = [x + 1]

-- Having in mind that we want to create a natural transformation, degenerateMtl2 should not work. We want to preserve the functionality that the function cannot do anything with the values, so we have a NATURAL TRANSFORMATION.
