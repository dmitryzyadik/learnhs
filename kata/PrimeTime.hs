module PrimeTime where

prime :: Int -> [Int]
prime n 
    | 1 == n = []
    | 0 == n = []
    | otherwise = sieve m m
    where         
        m = [2..n]
        

sieve :: [Int] -> [Int] -> [Int]
sieve f n 
    | f == [] = []
    | n == [] = f
    | otherwise = sieve (ffun h f) t
    where 
        h = head n
        t = tail n
        

ffun n m = filter ((\ a b -> b `mod` a /= 0 || a == b) n) m 