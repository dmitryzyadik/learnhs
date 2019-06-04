module TitleCase (titleCase) where
import Data.Char

titleCase :: String -> String -> String
titleCase minor title 
    | title == [] = ""
    | otherwise = (toUpper h) : wordCase minor t 
    where         
        h = head title 
        t = tail title

wordCase :: String -> String -> String
wordCase minor title     
    | title == []   = []
    | h == ' '      = ret ++ (wordCase minor tt)
    | otherwise     = (toLower h) : (wordCase minor t)
    where        
        h = head title 
        t = tail title
        ht = head t
        tt = tail t
        lowerT = map toLower t
        word = getWord lowerT
        lowerMinor = map toLower minor
        ret = h : (upOrLow ht word lowerMinor) : []

getWord :: String -> String
getWord title
    | title == [] = []
    | h == ' '  = []
    | otherwise = h : getWord t
    where        
        h = head title 
        t = tail title

upOrLow :: Char -> String -> String -> Char
upOrLow letter word sentense 
            | wordInSentense word sentense = toLower letter 
            | otherwise =  toUpper letter

wordInSentense :: String -> String -> Bool
wordInSentense word minor 
            | minor == []   = False
            | h == ' '  = if word == word' then True else wordInSentense word t 
            | word == word' = True
            | otherwise     = wordInSentense word t                 
            where 
                word' = getWord minor
                t = tail minor
                h = head minor



        
