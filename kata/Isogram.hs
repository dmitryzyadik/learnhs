module Isogram where
    import Data.Char
    
    isIsogram :: String -> Bool
    isIsogram str 
        | str == []  = True
        | (tail str) == [] = True
        | otherwise  = find' (map toLower (tail str)) (head str)
        where        
            find' :: [Char] -> Char -> Bool
            find' (s:str) c 
                | c == s  = False 
                | str       == []       = True            
                | otherwise             = (find' str c) && (find' str s)
         