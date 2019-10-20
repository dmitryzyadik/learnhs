module Trouble where

--import ClassyPrelude
import Data.List

upcaseFirst (c:cs) = toUpper c 

camelCase :: String -> String
camelCase xs = concat (map upcaseFirst (words xs))

