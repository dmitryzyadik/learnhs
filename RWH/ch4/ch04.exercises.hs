import Data.Char (digitToInt, isDigit, isSpace) 

type ErrorMessage = String

asInt_fold :: String -> Either ErrorMessage Int
asInt_fold [] = Left "empty"
asInt_fold ("-") = Left "no digit"
asInt_fold ('-':s) =  (neg  (asInt_fold s))
asInt_fold s =  (foldl step (Right 0) s )

neg :: (Either ErrorMessage Int) -> (Either ErrorMessage Int)
neg (Right x) = Right (negate x)

step (Left acc) x = Left acc
step (Right acc) x  | (isDigit x) = Right (acc * 10 + digitToInt x)
                    | otherwise  =  (Left "Not digit")


concat' :: [[a]] -> [a]
concat' = foldr (++) [] 


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | (f x) = x : (takeWhile' f xs)
                    | otherwise = (takeWhile' f xs)

takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr f [] = []
takeWhile_foldr f [x] | f x = [x]
takeWhile_foldr f  x = foldr take' [] x 
    where   take' x acc   | f x  = x : acc
                          | otherwise = acc

groupBy_foldr :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_foldr f x = reverse $ foldr step [] (reverse x)
    where   step x [[]] = [[x]]   
            step x (h@(hh:_):t)    | f hh x = (h ++ [x]) : t
            step x acc =  [x] : acc

groupBy_foldl :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_foldl f l = reverse $ foldl step [] l
    where   step (xs@(x:_):ys) e | f x e = (xs ++ [e]) : ys
            step acc e = [e] : acc                                
            

word_fold :: String -> [String]
word_fold s = foldr step [] s
    where step el [] = [[el]]
          step el acc@(h:t) | not . isSpace $ el = [el : h] ++ t   
                            | otherwise = []:acc          

unlines_fold :: [String] -> String
unlines_fold s = foldr (\e acc -> e ++ "\n" ++ acc) "" s
    
                                                