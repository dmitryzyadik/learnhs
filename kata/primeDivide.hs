sumOfDivided :: [Integer] -> [(Integer, Integer)]
sumOfDivided xs = map (sumOfDiv xs) (primeDivideM (map abs xs) )


sumOfDiv :: [Integer] -> Integer -> (Integer, Integer)
sumOfDiv mas d =  (d, sum $ filter ((\ d x -> x `mod` d == 0) d) mas)
    
isPrime k
    | k == 1 = False
    | k > 1000000 = True
    | otherwise = null [ x | x <- [2..k - 1], k `mod` x == 0]



primeDivide :: Integer -> Integer -> [Integer]
primeDivide probe n
    | n == 1            = []
    | probe > 1000000   = []
    | m /= 0            = primeDivide (probe + 1) n
    -- | isPrime d         = probe : d : primeDivide probe (n `div` probe)
    | otherwise         = probe : d: primeDivide probe (n `div` probe)
    where
        d = n `div` probe
        m = n `mod` probe
execPrimeDivide :: Integer -> [Integer]
execPrimeDivide = primeDivide 2

primeDivideM :: [Integer] -> [Integer]
primeDivideM = (quicksort . remDublicate . concat . map execPrimeDivide)

remDublicate :: [Integer] -> [Integer]
remDublicate x 
    | x == [] = []
    | exist h t = remDublicate t
    | otherwise = h : remDublicate t
    where 
        h = head x
        t = tail x

exist :: Integer -> [Integer] -> Bool
exist x m
    | m == []   = False
    | x == h    = True
    | otherwise = exist x t
    where 
        h = head m
        t = tail m

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smaller = quicksort [a | a <- xs, a <= x]
        bigger  = quicksort [a | a <- xs, a > x]
    in smaller ++ [x] ++ bigger