highAndLow :: String -> String
highAndLow str = show (maximum nums) ++ " " ++  show (minimum nums)
                    where nums = read' str ' ' []

read' :: [Char] -> Char -> [Char] -> [Int]
read' (x:xs) common acc
    | xs == [] = (read (acc ++ [x]) :: Int) : []
    | x == ' ' = (read acc :: Int) : (read' xs common [] )
    | otherwise = read' xs common (acc ++ [x])
    



{-| xs == [] = []
    | x == ' ' = acc : (split' xs common '' )
    | otherwise = split' xs common $ acc : x-}