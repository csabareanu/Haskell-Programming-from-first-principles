module Hello where

-- sayHello :: IO ()
-- sayHello = putStrLn "Hello from Haskell!"

sayHello :: String -> IO ()
sayHello name = putStrLn ("Hi " ++ name ++ "!")
