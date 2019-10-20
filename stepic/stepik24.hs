avg :: Int -> Int -> Int -> Double
avg a b c = ((toDouble a) + (toDouble b) + (toDouble c)) / 3

toDouble :: Int -> Double
toDouble a = fromIntegral a