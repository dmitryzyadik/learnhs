module Codewars.Kata.Dubstep where
import Data.Char

songDecoder :: String -> String
songDecoder dubstepString
    | dubstepString == []           = []
    | take 3 dubstepString == "WUB" = (cleanDubstepString (dropWub dubstepString "WUB" space) "WUB") 
    | otherwise                     = cleanDubstepString dubstepString "WUB"
    where
        space = []
cleanDubstepString :: String -> String -> String
cleanDubstepString dupstepStr wubStr
    | dupstepStr        == []               = []                
    | dupstepStr        == wubStr           = []                
    | xs                == wubStr           = x : []                
    | length dupstepStr < wubStrLen         = dupstepStr
    | take wubStrLen dupstepStr == wubStr   = (cleanDubstepString (dropWub dupstepStr wubStr space) wubStr)                    
    | otherwise                             = x : (cleanDubstepString xs wubStr)
    where 
        x   = head dupstepStr
        xs  = tail dupstepStr
        space = chr 32 : []
        wubStrLen = length wubStr

dropWub :: String -> String -> String -> String
dropWub str wub space
    | str == []         = []                        
    | str == wub        = []
    | length str <= wubStrLen   = (space ++ str)
    | take wubStrLen str == wub = dropWub (drop wubStrLen str ) wub space
    | otherwise         = space ++ str
    where
        wubStrLen = length wub
        
        



