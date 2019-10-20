module Main  where

import ClassyPrelude
import Lib 
import SimpleJSON

main :: IO ()
main = do
    print (JObject [("foo", JNumber 1), ("bar", JBool False)])
