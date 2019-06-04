module Codewars.Kata.SplitStrings where

solution :: String -> [String]
solution = f 
    where 
        f :: String -> [String]
        f a 
            | a == [] = [] 
            | t == [] = s h "_"
            | otherwise = s h t
                where 
                    h = head a
                    t = tail a  
        s :: Char -> String -> [String]
        s a b = (a : h : []) : f t 
                    where 
                        h = head b
                        t = tail b  
         