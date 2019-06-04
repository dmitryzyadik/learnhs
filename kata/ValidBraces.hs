module Codewars.Kata.Braces where

validBraces :: String -> Bool
validBraces xs 
    | xs == [] = False
    | isOpen h = findBraces (h:[]) t
    | otherwise = False
    where
        h = head xs
        t = tail xs

findBraces :: String -> String-> Bool
findBraces sHead sTail 
        | sHead /= [] && sTail == [] = False
        | sTail == [] = True
        | isOpen h  = findBraces (sHead ++ h:[]) t
        | sHead == [] && not (isOpen h)  = False
        | isOrder l h = findBraces i t
        | otherwise = False 
        where 
            h = head sTail
            t = tail sTail 
            l = last sHead 
            i = init sHead

isOpen :: Char -> Bool
isOpen a = (a == '(' || a == '[' || a == '{')
            

isOrder :: Char -> Char -> Bool
isOrder o c 
    | o == '(' && c  == ')' = True       
    | o == '[' && c  == ']' = True
    | o == '{' && c  == '}' = True
    | otherwise = False
    