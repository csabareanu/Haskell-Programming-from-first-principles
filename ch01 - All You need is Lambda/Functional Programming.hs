---- What is functional programming ?
-- Is a computer paradigm that uses functions modeled on mathematical functions

---------------------------
---- What are functions ?
---------------------------
-- Functions are expressions that are applied to an input, and once applied can be reduced or evaluated.
-- Can be seen as mapping a set of inputs to a set of outputs.
-- In Haskell:
-- 1) functions are FIRST CLASS (can be passed as arguments and can be used as values)
-- 2) Haskell is a PURE FUNCTIONAL LANGUAGE - all functions can be translated to lambda calculus
-- Obs. PURITY = REFERENTIAL TRANSPARENCY = a function, given the same values to evaluate will always return the same result (as in math).

--------------------
-- Lambda Calculus
--------------------
-- Three basic components:
-- A. Expressions (variable, abstraction or combination)
-- B. Variables (names for inputs to functions)
-- C. Abstractions ~ functions (Head (Lambda + Variable Name) + Body) - there are anonimous functions
--    Ex. 1) \x.x
--        2) (\x.x) 2 - BETA REDUCTION (If no reduction is possible it is in BETA NORMAL FORM)
--           2
--        3) (\x.x)(\y.y)   [x:=(\y.y)]
--           (\y.y)
--        4) (\x.x)(\y.y)z - this is LEFT ASSOC
--           ((\x.x)(\y.y))z [x:=(\y.y)]
--           (\y.y)z         [y:=z]
--           z
-- Types of variables in the body:
-- 1. Free variable - it is not named in the head. Alpha equivalence does not apply here (\x.xz)
-- Ex. (\x.xy)z
--     zy
-- 2. Bound variable - it is named in the head. A lambda term with no free variables is a COMBINATOR

-- Each lambda can only bind one parameter and can only accept one argument - currying.
-- \xy.xy <=> \x.(\y.xy)
-- Ex. 1) (\xy.xy)1 2 = (\x.(\y.xy))1 2 [[x:=1]] = (\y.1y) 2 = 12
--     2) (\xy.xy)(\z.a)1 = (\x.(\y.xy))(\z.a)1 [x:=(\z.a)] = \y.((\z.a)y)1 [y:=1] = (\z.a)1 [z:=1] = a
--     3) (\xyz.xz(yz))(\mn.m)(\p.p) = (\x.\y.\z.xz(yz))(\m.\n.m)(\p.p) [x:=(\m.\n.m)] = (\y.\z.(\m.\n.m)z(yz))(\p.p) [y:=\p.p]
--        = \z.(\m.\n.m)z((\p.p)z) [m:=z] = \z.(\n.z)((\p.p)z) [n:=(\p.p)z] = \z.z

---------------------------
-- Intermission: Exercises
----------------------------

-- We’ll give you a lambda expression. Keeping in mind both alpha
-- equivalence and how multiple heads are nested, choose an answer
-- that is equivalent to the listed lambda term.
-- 1. 𝜆𝑥𝑦.𝑥𝑧
-- a) 𝜆𝑥𝑧.𝑥𝑧
-- b) 𝜆𝑚𝑛.𝑚𝑧
-- c) 𝜆𝑧(𝜆𝑥).𝑥𝑧
-- R: b)

-- 2. 𝜆𝑥𝑦.𝑥𝑥𝑦
-- a) 𝜆𝑚𝑛.𝑚𝑛𝑝
-- b) 𝜆𝑥(𝜆𝑦).𝑥𝑦
-- c) 𝜆𝑎(𝜆𝑏).𝑎𝑎𝑏
-- R: c)

-- 3. 𝜆𝑥𝑦𝑧.𝑧𝑥
-- a) 𝜆𝑥.(𝜆𝑦).(𝜆𝑧)
-- b) 𝜆𝑡𝑜𝑠.𝑠𝑡
-- c) 𝜆𝑚𝑛𝑝.𝑚𝑛
-- R: b)










