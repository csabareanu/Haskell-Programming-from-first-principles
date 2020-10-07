module Print3Flipped where

myGreeting :: String
myGreeting = (++) "hello" " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where secondGreeting =
            (++) hello ((++) " " world)

-- secondGreeting is only visible withing the main function and not at the top level.

