module Return where

main :: IO Bool
main = do
    x1 <- getChar
    x2 <- getChar
    return (x1 == x2)