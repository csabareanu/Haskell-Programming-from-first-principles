{-# LANGUAGE FlexibleInstances #-}

module Chapter_ex where

import GHC.Arr

----------------------------------------------------------------------------
-- Determine if a valid Functor can be written for the datatype provided.
----------------------------------------------------------------------------
-- 1.
data Bool =
    False | True

-- No Functor instance because kind is *

-- 2.
data BoolAndSomethingElse a =
    False' a | True' a
    deriving (Show)

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' (f a)
    fmap f (True' a)  = True' (f a)

-- 3.
data BoolAndMaybeSomethingElse a =
    Falsish | Truish a
    deriving Show

instance Functor BoolAndMaybeSomethingElse where
    fmap _ Falsish    = Falsish
    fmap f (Truish a) = Truish (f a)

-- 4. Use the kinds to guide you on this one, don’t get too hung up on
-- the details.

newtype Mu f =
    InF { outF :: f (Mu f) }

-- No Functor. Kind is: (* -> *) -> *

--5. Again, just follow the kinds and ignore the unfamiliar parts


data D =
    D (Array Word Word) Int Int

-- No Functor. D has kind *

--------------------------------------------------------------------------
-- Rearrange the arguments to the type constructor of the datatype so
-- the Functor instance works.
--------------------------------------------------------------------------
-- 1.
-- data Sum a b =
--     First a
----     | Second b
data Sum  b a =
    First a
    | Second b


instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b


-- 2.
-- data Company a b c =
--     DeepBlue a c
----     | Something b
data Company a c b =
    DeepBlue a c
    | Something b
instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c


-- 3.
-- data More a b =
--     L a b a
----     | R b a b
--     deriving (Eq, Show)
data More b a =
    L a b a
    | R b a b
    deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

--Keeping in mind that it should result in a Functor that does the
-- following:
-- Prelude> fmap (+1) (L 1 2 3)
-- L 2 2 4
-- Prelude> fmap (+1) (R 1 2 3)
-- R 1 3 3


--------------------------------------------------------
-- Write Functor instances for the following datatypes.
--------------------------------------------------------
-- 1.
data Quant a b =
    Finance
    | Desk a
    | Bloor b
    deriving Show

instance Functor (Quant a) where
    fmap _ Finance   = Finance
    fmap _ (Desk a)  = Desk a
    fmap f (Bloor b) = Bloor (f b)


-- 2. No, it’s not interesting by itself.
data K a b =
    K a
    deriving Show

-- instance Functor (K a) where
--     fmap _ (K a) = K a

-- 3. {-# LANGUAGE FlexibleInstances #-}
newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

-- newtype K a b =
--     K a
-- -- should remind you of an
-- -- instance you've written before -- page 642

instance Functor (Flip K a) where
    fmap f (Flip (K b)) = Flip $ K (f b)


-- 4.
data EvilGoateeConst a b =
    GoatyConst b
-- You thought you'd escaped the goats
-- by now didn't you? Nope.
-- No, it doesn’t do anything interesting. No magic here or in the
-- previous exercise. If it works, you succeeded.
instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)


--5. Do you need something extra to make the instance work?
-- f must have a functor instance to fmap over it
data LiftItOut f a =
    LiftItOut (f a)
    deriving Show


instance Functor f => Functor (LiftItOut f) where   -- *Chapter_ex> :k LiftItOut Maybe :: * -> * (Correct Kind)
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)


-- 6.
data Parappa f g a =
    DaWrappa (f a) (g a)
    deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- *Chapter_ex> fmap (+1) (DaWrappa (Just 1) Nothing)
-- DaWrappa (Just 2) Nothing
-- *Chapter_ex> fmap (+1) (DaWrappa (Just 1) (Just 5))
-- DaWrappa (Just 2) (Just 6)
-- *Chapter_ex> fmap (+1) (DaWrappa (Just 1) (Right 3))
-- DaWrappa (Just 2) (Right 4)
-- *Chapter_ex> fmap (+1) (DaWrappa (Just 1) (Left "error"))
-- DaWrappa (Just 2) (Left "error")


-- 7. Don’t ask for more typeclass instances than you need. You can
-- let GHC tell you what to do.
data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)
    deriving Show

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- *Chapter_ex> fmap (+1) (IgnoringSomething (Just 1) (Left 1))
-- IgnoringSomething (Just 1) (Left 1)
-- *Chapter_ex> fmap (+1) (IgnoringSomething (Just 1) (Right 1))
-- IgnoringSomething (Just 1) (Right 2)


-- 8.
data Notorious g o a t =
    Notorious (g o) (g a) (g t)
    deriving Show

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- *Chapter_ex> fmap (+1) (Notorious (Left 1) (Right 1) (Left 1))
-- Notorious (Left 1) (Right 1) (Left 1)
-- *Chapter_ex> fmap (+1) (Notorious (Left 1) (Right 1) (Right 1))
-- Notorious (Left 1) (Right 1) (Right 2)


-- 9. You’ll need to use recursion.
data List a =
    Nil
    | Cons a (List a)
    deriving Show

instance Functor List where
    fmap _ Nil               = Nil
    fmap f (Cons head tail)  = Cons (f head) (fmap f tail)

-- *Chapter_ex> fmap (+1) (Cons 1 ((Cons 2) Nil))
-- Cons 2 (Cons 3 Nil)


--10. A tree of goats forms a Goat-Lord, fearsome poly-creature.
data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving Show
-- A VERITABLE HYDRA OF GOATS

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat l) = OneGoat (f l)
    fmap f (MoreGoats l c r) = MoreGoats (fmap f l) (fmap f c) (fmap f r)

-- *Chapter_ex> fmap (+1) (MoreGoats (NoGoat) (OneGoat 1) (MoreGoats (NoGoat) (OneGoat 2) (OneGoat 4)))
-- MoreGoats NoGoat (OneGoat 2) (MoreGoats NoGoat (OneGoat 3) (OneGoat 5))

--11. You’ll use an extra functor for this one, although your solution
--might do it monomorphically without using fmap.
data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)

instance (Show a) => Show (TalkToMe a) where
    show Halt             = "Halt"
    show (Print a b)      = "Print " ++ a ++ " " ++ show b
    show (Read g)         = "Read"

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g) = Read (fmap f g)