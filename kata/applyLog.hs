import Data.Monoid
import Control.Monad.Writer

type Food = String
type Price = Sum Int
--newtype Writer w a = Writer {runWriter :: (a, w) }

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

addDrink :: Food -> (Food, Price)
addDrink "beans"    = ("milk", Sum 25)
addDrink "jerky"    = ("whiskey", Sum 99)
addDrink _          = ("beer", Sum 30)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna mltiply these two"]
    return (a*b)

{- instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
 -}