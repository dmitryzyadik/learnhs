disemvowel :: String -> String
disemvowel str
    | str == []         = []
    | notVowel' vowel x = x : disemvowel (tail str) 
    | otherwise         = disemvowel (tail str) 
    where   vowel= "aeyuio"
            x = head str
            notVowel' :: [Char] -> Char -> Bool
            notVowel' (x:xs) a         
                | x  == a   = False
                | xs == []  = True   
                | otherwise = notVowel' xs a


            