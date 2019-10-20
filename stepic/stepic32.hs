import Data.Char 

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\ x y z -> maximum [x,y,z])


perms :: [a] -> [[a]]
perms x = switch' [] (length x) x
    where
        switch' ::  [a] -> Int -> [a] -> [[a]]
        switch' acc 0 []        = [acc]
        switch' acc 0 _         = []
        switch' acc n (x:xs)    = switch' (acc ++ [x]) (length xs) xs  ++ switch' acc (n-1)  (xs++[x])  

--perms [1,2,3]
--[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

delAllUpper :: String -> String
delAllUpper w = unwords $ filter (not . and . map (\x -> (ord x) > 64 && (ord x) < 90)) $ words w 