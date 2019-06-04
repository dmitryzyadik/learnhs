module IsPrime where

isPrime :: Int -> Bool
isPrime x
        | x == 0 = False
        | x == 1 = False
        | x == 2 = True
        | x == 3 = True
        | x `div` 2 == 0 = False
        | x `div` 3 == 0 = False        
        | [] == filter ((\ a b -> a `mod` b == 0 ) x) m = True
        | otherwise = False
        where 
            m = [2..(abs x-1)]