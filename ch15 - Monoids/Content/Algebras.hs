-- Algebras : One or more operations and the set they operate over (set = type)
-- In Haskell they can be implemented with typeclasses
-- The typeclasses define the set of operations
-- The set is the type the operations are for.
-- The instance defines how each operation will perform for a given type or set

--------------
--MONOIDS
--------------
-- A Monoid is a binary associative operation with an identity.
-- A Monoid is a function that takes 2 args and follows 2 laws: associative and identity.

-- The Monoid typeclass has a pattern of types that have binary functions that let you join things together in accordance to the laws of associativity,
-- along with an id value that returns the other argument unmodified
-- Ex. summation, multiplication, list concatenation, etc. - this typeclass abstracts the pattern out.

-------
--List
-------
-- mempty = []
-- mappend = (++)

-- Integer does not have a Monoid instance because there are 2 operations that are monoidal (* and +) and each type should have one unique instance for a given typeclass
-- mappend 1 2 -> it is not clear if we want the values to be added or multiplied
-- To resolve this we have Sum and Product newtypes to wrap numeric values and signal which Monoid instance we want.

--  Mappending is perhaps best thought of not as a way of
--combining values in the way that addition or list concatenation does,
--but as a way to condense any set of values to a summary value

----------------
-- Bool Monoids
----------------
-- Conjuction: (ALL) True if and only if ALL values it is "appending" are True.
-- Disjunction: (ANY) True if any value it appends is True.

-----------------
-- Maybe Monoids
-----------------
-- 1) First and Last - First or last value that is not Nothing or Nothing


-----------------
-- SEMIGROUPS----
-----------------

-- A semigroup is a binary associative operation (It's a monoid without the identity property)

-- class Semigroup a where
--   (<>) :: a -> a -> a

-- The rule that a monoid must obey:
-- (a <> b) <> c == a <> (b <> c)
