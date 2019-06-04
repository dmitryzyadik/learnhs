import Data.Char (ord , isDigit)

nTimes:: a -> Int -> [a] 
nTimes _ 0 = []
nTimes x n = x : nTimes x (n-1)

--((,) y x : z) +
--((,) x y : z) -
--((_, x) : _) +
--((:) ((,) _ x) y) +
--((,) ((:) _ _) x) -
--((,) y z : x) -
sndHead ((,) x y : z) = x

--f = sndHead [(1,2), (3,4)]

oddsOnly :: Integral a => [a] -> [a]
oddsOnly []     = []
oddsOnly (x:xs) = if odd x then x : oddsOnly xs else oddsOnly xs   

isPalindrome :: Eq a => [a] -> Bool
--isPalindrome x = if (reverse x == x ) then True else False
isPalindrome = \x -> reverse x == x

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 xs [] [] = xs 
sum3 [] ys [] = ys 
sum3 [] [] zs = zs 
sum3 (x:xs) (y:ys) [] = (x + y) : sum3 xs ys [] 
sum3 (x:xs) [] (z:zs) = (x + z) : sum3 xs [] zs 
sum3 [] (y:ys) (z:zs) = (y + z) : sum3 [] ys zs 
sum3 (x:xs) (y:ys) (z:zs) = (x + y +z) : sum3 xs ys zs 

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = f [x] xs
    where
        f :: Eq a => [a] -> [a] -> [[a]]
        f a [] = [a]
        f (a:ax) (b:bx) 
            | a == b  = f (aa ++ [b]) bx
            | otherwise = aa : (f [b] bx)
            where 
                aa = a : ax

groupElems' :: Eq a => [a] -> [[a]]
groupElems' [] = []
groupElems' (x:xs) = (takeWhile (==x) (x:xs)) : groupElems' (dropWhile (==x) xs)
    {-where
        f :: Eq a => [a] -> [a] -> [[a]]
        f a [] = [a]
        f a b = a : f (takeWhile (==h) b) (dropWhile (==h) b)
            where 
                h = head b-}
readDigits :: String -> (String, String)
readDigits = break (\x -> ord x > 57) 

readDigits' :: String -> (String, String)
readDigits' = break  isDigit
                            
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a] 
filterDisj a b [] = []
filterDisj a b (x:xs) 
    | a x || b x    = [x] ++ (filterDisj a b xs)                                 
    | otherwise     = (filterDisj a b xs)                                 
--filterDisj (< 10) odd [7,8,10,11,12]
