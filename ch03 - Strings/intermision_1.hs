-- 1. These lines of code are from a REPL session. Is 𝑦 in scope for 𝑧?
-- Prelude> let x = 5
-- Prelude> let y = 7
-- Prelude> let z = x * y
-- R: yes

-- 2. These lines of code are from a REPL session. Is ℎ in scope for
-- function 𝑔?
-- Prelude> let f = 3
-- Prelude> let g = 6 * f + h
-- R: no

-- 3. This code sample is from a source file. Is everything we need to
-- execute area in scope?
-- area d = pi * (r * r)
-- r = d / 2
-- R: no

-- 4. This code is also from a source file. Now are 𝑟 and 𝑑 in scope for
-- area?
area d = pi * (r * r)
    where r = d / 2

-- R: yes
