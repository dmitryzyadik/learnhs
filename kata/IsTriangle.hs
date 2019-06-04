isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c 
    | m < (sum [a, b, c] - m)   = True
    | otherwise                 = False 
    where 
        m = maximum [a, b, c]