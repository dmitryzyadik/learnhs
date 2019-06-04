import Control.Monad.State.Lazy
import Control.Monad.Reader

g = runState get 5
p = runState (put 7) 5

tick :: State Int Int
tick = do
    n <- get
    put (n+1)
    return n

t = runState tick 5

modifi :: (s -> s) -> State s ()
modifi f = do
    s <- get
    put (f s)
    return ()

m = runState (modifi (^2)) 5


readerToState :: Reader r a -> Reader r a
readerToState m = local m