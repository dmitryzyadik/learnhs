productFib :: Integer -> (Integer, Integer, Bool)
productFib n = fib 0 1 n


fib :: (Ord a, Num a) => a -> a -> a -> (a, a, Bool)
fib a b n 
    | (a * b) == n = (a,b,True)
    | (a * b) > n = (a,b,False)    
    | otherwise = fib b (a+b) n



