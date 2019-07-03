module Main where

main :: IO ()
main = do
  putStrLn "hello world"

primes :: [Integer]
primes = 2 : filterMultiples allMultiples [3,5..]
  where
    allMultiples = mergeMultiples $ map multiples primes
    multiples i = map (i*) [i..]
