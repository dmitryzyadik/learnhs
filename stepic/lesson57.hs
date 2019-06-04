import Data.Monoid

{- newtype Writer w a = Writer {runWriter :: (a, w)}

writer :: (a, w) -> Writer w a 
writer = Writer

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    m >>= k =
        let (x, u) = runWriter m
            (y, v) = runWriter $ k x
        in Writer (y, u `mappend` v)

tell :: Monoid w => w -> Writer w ()
tell w = writer ((), w) -}
import Control.Monad.Writer

calc :: (Int -> Int -> Int ) -> Int -> Int -> Writer String Int
calc op arg1 arg2 = do
    let res = arg1 `op` arg2
    tell "ok"
    if abs res < 128 then
        return res
    else do
        tell "overflow"
        return res

type Shopping = Writer (Sum Integer) ()

shopping1 :: Shopping
shopping1 = do
    purchase "Jeans"   19200
    purchase "Water"     180
    purchase "Lettuce"   328

purchase :: String -> Integer -> Shopping
purchase item cost = do    
    return cost :: Writer (Sum Integer) ()

total :: Shopping -> Integer
total s= getSum $ snd (runWriter s)