data Reader r a = Reader { runReader :: (r -> a) }

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

instance Functor (Reader e) where
    fmap f mr = Reader (\r -> f (runReader mr r))

instance Applicative (Reader e) where
    pure x = Reader (\_ -> x)
    rf <*> rx = Reader (\r -> (runReader rf r) (runReader rx r))

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \e -> runReader m (f e) 