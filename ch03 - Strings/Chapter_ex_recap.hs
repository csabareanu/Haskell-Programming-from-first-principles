module Chapter_ex2 where

rvrs :: String
rvrs = drop 9 word ++ take 4 (drop 5 word)  ++ take 5 word
    where word = "Curry is awesome"