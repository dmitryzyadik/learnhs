gps :: Int -> [Double] -> Int

gps s [] = 0
gps s [x] = 0
gps s (x:xs) = maximum $ help' x xs
    where
        help' :: Double -> [Double] -> [Int]
        help' p [] = []
        help' p (x:xs) = (round (3600 * (x-p) / fromIntegral(s) )) : help' x xs
        
        

