

import qualified Data.Char(isSpace)


decodeMorse :: String -> String
decodeMorse [] = []
decodeMorse m =  unwords2 $ words2 $ dropWhile Data.Char.isSpace m
        

words2            :: String -> [String]
words2 s          =  case s of
                      "" -> []
                      s' -> w' : w : words2 s''
                            where 
                            (w, s'')    = break Data.Char.isSpace s'''  
                            (w', s''')  = span Data.Char.isSpace s'  
-- unwords2          :: [String] -> String
unwords2 []       =  ""
unwords2 ws       =  foldr (sp) "" ws

sp w s = case w of 
    ""    -> s
    " "   -> s
    "  "  -> s
    "   " -> ' ' : s            
    w'    -> (morseCodes' w') ++ s                            

morseCodes' :: String -> String
morseCodes' ".-" = "A"
morseCodes' "-..." = "B"
morseCodes' "-.-." = "C"
morseCodes' "-.." = "D"
morseCodes' "." = "E"
morseCodes' "..-." = "F"
morseCodes' "--." = "G"
morseCodes' "...." = "H"
morseCodes' ".." = "I"
morseCodes' ".---" = "J"
morseCodes' "-.-" = "K"
morseCodes' ".-.." = "L"
morseCodes' "--" = "M"
morseCodes' "-." = "N"
morseCodes' "---" = "O"
morseCodes' ".--." = "P"
morseCodes' "--.-" = "Q"
morseCodes' ".-." = "R"
morseCodes' "..." = "S"
morseCodes' "-" = "T"
morseCodes' "..-" = "U"
morseCodes' "...-" = "V"
morseCodes' ".--" = "W"
morseCodes' "-..-" = "X"
morseCodes' "-.--" = "Y"
morseCodes' "--.." = "Z" 
morseCodes' "...---..." = "SOS"
morseCodes' ".-.-.-" = "." 
morseCodes' "-.-.--" = "!"
morseCodes' "     " = ""