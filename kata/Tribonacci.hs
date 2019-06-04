module Tribonacci where

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n = take n $ a : b : c : [] ++ f a b c
        where             
            f h1 h2 h3 = nh3 : [] ++ f nh1 nh2 nh3
                where 
                    nh1 = h2  
                    nh2 = h3
                    nh3 = (h1 + h2 + h3)

