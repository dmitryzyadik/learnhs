-- file ch5/Trouble.hs
import Data.Char (toUpper)

upcaseFirst (c:cs) = toUpper c -- forgor ":cs" here

camelCase :: String -> String
camelCase xs = concat (map upcaseFirst (words xs))
