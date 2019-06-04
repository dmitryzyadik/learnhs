import Data.IntMap.Strict ((!), fromList, insert)

explosiveSum :: Integer -> Integer
explosiveSum n = f n n 
    
f :: Integer -> Integer -> Integer       
f n k 
    | n == 0 && k==0    = 1
    | n /= 0 && k==0    = 0
    | k > n   = f n n
    | otherwise         = (f n (k-1)) + (f (n-k) k )


generatePs :: (Int,[Int]) -> [[Int]] 
generatePs (n,[])       = [take n (repeat 1)] 
generatePs (n,(x:xs))   = 
    (take n (repeat 1) ++ (x:xs)) : generatePs (pack (x-1) ((n+x),xs)) 
    where 
    pack :: Int -> (Int,[Int]) ->(Int,[Int]) 
    pack 1 (m,xs) = (m,xs) 
    pack k (m,xs) = if k > m  then pack (k-1) (m,xs) 
                    else           pack k     (m-k,k:xs) 

parts :: Int -> [[Int]] 
parts n | n < 1     = error "part: argument <= 0" 
        | n == 1    = [[1]] 
        | otherwise = generatePs (0,[n]) 



partition :: Int -> Integer
partition x = if x < 0 then 0 else foldl p (fromList [(0,1)]) [1..x] ! x 
    where
    p s n = insert n (sum [sign k * sumOfTwoEarlierPs s n k | k <- [1..maxK n] ]) s
    maxK n = round $ 1/6 + sqrt(1/36 + 2/3 * fromIntegral n)

sign k = if odd k then 1 else (-1)

sumOfTwoEarlierPs s n k = p1 + p2 where
    i1 = n - (k *(k+k+k - 1)) `div` 2
    i2 = i1 - k
    p1 = nonNegativeIndexOnly s i1
    p2 = nonNegativeIndexOnly s i2
    nonNegativeIndexOnly s i = if i >= 0 then s!i else 0

explosiveSum' :: Integer -> Integer
explosiveSum' n | n < 0 = 0
explosiveSum' n = go 1 n
    where   go k m = mem' !! (fromIntegral k) !! (fromIntegral m)
            mem' = [[p k m |m <- [0..n+2]] | k <- [0..n+2] ]
            p _ 0 = 1
            p k m | m < k = 0
            p k m = go k (m - k) + go (k + 1) m