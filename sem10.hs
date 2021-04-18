module Main where

-- 1. Отличие Applicative и Monad

ifThenElseM :: Monad m => m Bool -> m a -> m a -> m a
ifThenElseM condM thenM elseM = do
    cond <- condM
    if cond 
      then thenM
      else elseM

ifThenElseA :: Applicative f => f Bool -> f a -> f a -> f a
ifThenElseA condA thenA elseA = ifThenElse <$> condA <*> thenA <*> elseA
  where
    ifThenElse :: Bool -> a -> a -> a
    ifThenElse cond th el = if cond then th else el

-- 2. Applicative и Functor через Monad

myFmap :: Monad m => (a -> b) -> m a -> m b
myFmap f ma = do
    x <- ma
    pure $ f x

myApp :: Monad m => m (a -> b) -> m a -> m b
myApp fM ma = do
    f <- fM
    a <- ma
    pure $ f a

-- 3. Дешугаринг

-- term a b = do 
--     a
--     b 
--     if 3 < 5
--         then do
--             a
--             pure 4 
--         else pure 5
--     x <− a 
--     let y = 5
--     [x, y] <− b
--     [x, y, z] <− a
--     x <− x x
--     pure x

-- term' a b = 
--   a >> b 
--     >> (if 3 < 5 then a >> pure 4 else pure 5) 
--     >> a 
--     >>= (\x -> 
--         let y = 5 in 
--         b >>= (\b' ->
--           case b' of
--             [x, y] -> a >>= (\a' -> 
--                               case a' of
--                                 [x, y, z] -> x x >>= (\x -> pure x)
--                                 _         -> fail ""
--                             )
--             _      -> fail ""
--                )
--         )

-- 4. IO

main :: IO ()
main = do
    putStrLn "Hello, put your name here:" :: IO ()
    userName <- getLine :: IO String
    putStrLn $ "Hello, " <> userName <> "!"
