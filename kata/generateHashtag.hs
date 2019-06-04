import Data.Char


generateHashtag :: String -> Maybe String
generateHashtag s 
    | l > 140   = Nothing
    | l == 1     = Nothing
    | otherwise = Just r
    where
        r = "#" ++ foldr (\(x:xs) y -> [(toUpper x)]++xs++y) "" (words s)      
        l = length r