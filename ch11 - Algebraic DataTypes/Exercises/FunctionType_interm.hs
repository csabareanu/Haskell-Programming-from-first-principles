module FunctionType_interm where


--Consider the following function:
convert :: Quantum -> Bool
-- convert = undefined
-- According to the equality of a -> b and 𝑏𝑎 there should be 2^3 or 8
-- implementations of this function. Does this hold? Write it out and
-- prove it for yourself.
convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = False
convert4 No = True
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = False
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = True
convert6 No = False
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = True
convert7 Both = False

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False


-- Determine how many unique inhabitants each type has.
-- Suggestion: just do the arithmetic unless you want to verify. Writing
-- them out gets tedious quickly.
-- 1.
data Quad =
    One
    | Two
    | Three
    | Four
    deriving (Eq, Show)
-- 4
-- how many different forms can this take?
-- eQuad :: Either Quad Quad
-- eQuad = ???
-- 4 + 4 = 8

-- 2. prodQuad :: (Quad, Quad)
-- 4 * 4 = 16

-- 3. funcQuad :: Quad -> Quad
-- 4^4 = 256

-- 4. prodTBool :: (Bool, Bool, Bool)
-- 2*2*2 = 8

-- 5. gTwo :: Bool -> Bool -> Bool
-- 2 ^ 2 ^ 2 = 16
-- 2 ^ (2*2) = 2^4 = 16

-- 6. Hint: 5 digit number
-- fTwo :: Bool -> Quad -> Quad
-- 2 ^ 4 ^ 4 = 2 ^ 16 = 65.536
-- 4^(4*2) = 65536