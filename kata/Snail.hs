module Snail where

snail :: [[Int]] -> [Int]
snail s = (getTop s) ++ (getRight m)  ++ (getBottom s)  ++ (getLeft m)
    where 
        h = head s
        m = init . tail $ s

cutSnail :: [[Int]] -> [[Int]]        
cutSnail s = 
    where
        m = init . tail $ s
        mm = 

main = snail [[1,2,3],[4,5,6],[7,8,9]]
    
getTop :: [[Int]] -> [Int]
getTop s = head s        

getBottom s = reverse (last s)             

{-send tail s-}
getRight :: [[Int]] -> [Int]
getRight s 
    | s == [] = []
    | otherwise = l : getRight t
    where 
        t = tail s
        h = head s
        l = last h 

getLeft :: [[Int]] -> [Int]
getLeft s 
    | s == [] = []
    | otherwise = (getLeft t) ++ (h:[])
    where 
        t = tail s 
        h = head . head $ s
