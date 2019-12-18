--import Data.ByteString.Char8
module GlobRegex
    (
        globToRegex
        , matchesGlob
    ) where

import Text.Regex.Posix ((=~))
import Data.Char (toLower)

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' ""             = ""
globToRegex' ('*':cs)       = ".*" ++ globToRegex' cs
globToRegex' ('?':cs)       = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = '[' : c : charClass cs
globToRegex' ('[':_)        = error "unterminated character class"
globToRegex' (c:cs)         = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChar   = '\\' : [c]
         | otherwise            = [c]
    where regexChar             = "\\+()^$.{}]|"

charClass :: String -> String    
charClass (']':cs)  = ']' : globToRegex' cs
charClass (c:cs)    = c : charClass cs
charClass []        = error  "unterminated character class"

matchesGlob :: Bool -> FilePath -> String -> Bool
matchesGlob True name pat = (map toLower name) =~ globToRegex (map toLower pat)
matchesGlob False name pat = name =~ globToRegex pat