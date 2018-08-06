module NormalForm where

data Fiction = Fiction deriving Show
data NonFiction = NonFiction deriving Show

data BookType = FictionalBook Fiction
                | NonFictionalBook NonFiction
                deriving Show

type AuthorName = String

-- not in normal form because BookTYpe is a sum type (a or b) -> a * (b + c) = a * b + a * c
-- data Author = Author (AuthorName, BookType)

-- now is in Normal Form :
data Author = Fiction AuthorName
              | NonFiction AuthorName
