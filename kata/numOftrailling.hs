zeros :: Int -> Int
zeros n  
  | n < 5 = 0 
  | otherwise = d5 + zeros d5
    where
      d5 = n `div` 5
      