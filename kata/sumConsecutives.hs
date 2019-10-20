sumConsecutives :: [Int] -> [Int]
sumConsecutives [] = []
sumConsecutives (x:xs) = helper x x xs
  
helper :: Int -> Int -> [Int] -> [Int]
helper cond sum [] = [sum]
helper cond sum (x:xs)
                | cond == x = helper cond (sum + x) xs
                | otherwise  = sum : (helper x x xs)