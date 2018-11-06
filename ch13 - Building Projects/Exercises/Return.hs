module Return where

main :: IO ()
main = do
    x1 <- getChar
    x2 <- getChar
    if x1 == x2
        then putStrLn "True"
        else return ()