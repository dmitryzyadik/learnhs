module Test where
--import Data.List hiding (union)
--import Data.Set

--myUnion [] ys = ys
--myUnion xs ys = union xs ys

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y xs = x : y : xs 