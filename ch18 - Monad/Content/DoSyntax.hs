module DoSyntax where

-- is just syntactic sugar.
-- Works with any monad, not just IO

-- (*>) :: Applicative f => f a -> f b -> f b
-- (>>) :: Monad m => m a -> m b -> m b

-- (*>) and (>>) are practically the same thing.

--import Control.Applicative ((*>))

sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
    putStrLn "blah" *>
    putStrLn "another thing"

binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

binding' :: IO ()
binding' =
    getLine >>= putStrLn


-- THIS DOES NOT WORK AS EXPECTED:
-- *DoSyntax> putStrLn <$> getLine
-- WHY ??
-- *DoSyntax> :t putStrLn <$> getLine
-- putStrLn <$> getLine :: IO (IO ())

-- *DoSyntax> :t getLine
-- getLine :: IO String

-- *DoSyntax> :t putStrLn
-- putStrLn :: String -> IO ()

-- *DoSyntax> :t (<$>)
-- (<$>) :: Functor f => (a      -> b    ) -> f a -> f b
-- putStrLn ::            String -> IO ()

-- f :: Functor f => f String -> f (IO ())
-- f x = putStrLn <$> x

-- g :: (String -> b) -> IO b
-- g x = x <$> getLine

-- putStrLn <$> getLine :: IO (IO ())

-- In order to evaluate the expression correctly, we need to JOIN the 2 IO layers together
-- *DoSyntax> import Control.Monad
-- *DoSyntax Control.Monad> join $ fmap putStrLn getLine

-- *DoSyntax Control.Monad> :t join $ fmap putStrLn getLine
-- join $ fmap putStrLn getLine :: IO ()
