sum' :: (Num a ) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

{-maxinum' :: (Ord a) => [a] -> a
maximum'  max = foldl1 max -}

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p= foldr (\x acc -> if p x then x : acc else acc) []

list' :: [a] -> a
list' = foldl1 (\_  x -> x)

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs
