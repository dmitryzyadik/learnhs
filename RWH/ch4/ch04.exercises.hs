import Data.Char (digitToInt, isDigit) 

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

{- int as_int(char *str)
{
    int acc; /* accumulate the partial result */

    for (acc = 0; isdigit(*str); str++) {
	acc = acc * 10 + (*str - '0');
    }

    return acc;
} -}