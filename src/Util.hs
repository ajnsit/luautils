module Util where

-- Utility functions

-- | "whileIterateM b f a" will execute action (f a) while (b a) is true
--   and also feed the results back to the next iteration.
whileIterateM :: Monad m => (a -> m Bool) -> (a -> m a) -> a -> m a
whileIterateM b f a = ifM (b a) (f a >>= whileIterateM b f) (return a)

-- | Monadic version of the if condition
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = b >>= bool t f

-- | Bool deconstructor in the spirit of 'either' and 'maybe'
--   Similar to the lambda-if proposal
bool :: a -> a -> Bool -> a
bool a b p = if p then a else b

forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ l f = mapM_ f l
