module Reverse where

reverseWords :: String -> String
reverseWords str
    | str == []         = []
    | t == []           = h : []
    | h == space        = h : reverseWords t 
    | lenStr == lenWord = revWord
    | otherwise         = revWord ++ (reverseWords (drop lenWord str))    
    where 
        h       = head str
        t       = tail str
        space   = ' '
        lenStr  = length str
        revWord = getReverseWord str
        lenWord = length revWord


getReverseWord :: String -> String
getReverseWord str 
    | str == []     = []
    | h == space    = []
    | otherwise     = (getReverseWord t) ++ h:[]  
    where 
        h       = head str
        t       = tail str
        space   = ' '

