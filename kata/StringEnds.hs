solution :: String -> String -> Bool
solution a b = (take n $ reverse a) == (reverse b) where n = length b