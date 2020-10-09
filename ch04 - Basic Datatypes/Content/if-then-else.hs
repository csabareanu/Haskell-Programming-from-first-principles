-- the structure is :
-- if CONDITION then EXPRESSION_A else EXPRESSION_B
-- condition must be of type bool
-- the type of the expression in then and else clauses must be the same

-- Prelude> let x = 0
-- Prelude> if (x + 1 == 1) then "AWESOME" else "wut"
-- "AWESOME"

greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool
        then putStrLn "yeeeey"
        else putStrLn "pshhhh"
    where cool = coolness == "down"
