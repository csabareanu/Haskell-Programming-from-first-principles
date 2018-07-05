module Integral_interm where


-- Look at the types given for quotRem and
-- divMod. What do you think those functions do? Test your hypotheses
-- by playing with them in the REPL. We’ve given you a sample to start
-- with below:
-- Prelude> let ones x = snd (divMod x 10)




-- Prelude> :t quotRem
-- quotRem :: Integral a => a -> a -> (a, a)
-- Prelude> quotRem 3 2
-- (1,1)  // similar to C++ % and mod (%,mod) . -> division with truncation towards 0
-- Ex. Prelude> quotRem (-12) 5
--     (-2,-2)

-- Prelude> :t divMod
-- divMod :: Integral a => a -> a -> (a, a)
-- divMod  (-12) 5
-- (-3,3)  // convention of number theorists -> division with truncation towards -Infinity