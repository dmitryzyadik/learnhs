data Odd = Odd Integer   deriving (Eq, Show)

instance Enum Odd where
   --toEnum           :: Int -> a
   toEnum n           = Odd (toInteger n)
   --fromEnum         :: a -> Int
   fromEnum (Odd  n)    =(fromIntegral n)
   --succ       :: Enum Odd -> Enum Odd
   succ (Odd m)        = Odd (m+2)
   --pred       :: a -> a
   pred (Odd m)        = Odd (m-2)
   enumFrom  (Odd m)   = (Odd m) : enumFrom  (Odd (m+2)) 

   enumFromThen (Odd n) (Odd n') = (Odd n) : enumFromThen (Odd next) (Odd next')
      where 
         next'  = n' + (n' - n)
         next = n' 

   enumFromTo (Odd n) (Odd n') 
         | n > n' = []
         | otherwise = (Odd n) : enumFromTo  (Odd (n+2)) (Odd n')

   enumFromThenTo (Odd n) (Odd n') (Odd n'')
      | abs(n) == abs(n'') = [(Odd n)]
      | n > n'' && n'' > n' = [(Odd n),(Odd n'')]
      | n < n'' && n'' < n' = [(Odd n),(Odd n'')]
      | otherwise =  (Odd n) : enumFromThenTo (Odd next) (Odd next') (Odd n'')
         where 
            next'  = n' + (n' - n)
            next = n' 

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd (x) then x + s else s) 0

meanList :: [Double] -> Double
meanList  d = (\(n,s) -> (s/n)) $ foldr (\x (n,s) -> (n+1, s+x)) (0.0,0.0) d

evenOnly :: [a] -> [a]
evenOnly d = reverse $ snd (foldr (\x (n, xs) -> if n then (not n, x:xs) else (not n,xs) ) (False,[]) (reverse d))

 