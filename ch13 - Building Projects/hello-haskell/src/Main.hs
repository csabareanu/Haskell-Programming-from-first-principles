module Main where

import System.IO

import Hello

-- it must have a main function and the file must be named Main. The main function must be of type IO a where a is a type, often () - unit .
main :: IO ()
-- main = sayHello
main = do
    hSetBuffering stdout NoBuffering -- used s.t. putStr isn't buffered (deferred) and prints immediately
    putStr "Please input your name: "
    name <- getLine --getLine has type IO String , <- is bind. The result of binding over a IO String is String and is bounded to variable name
    sayHello name
