fibStream :: [Integer]
fibStream = zipWith (+) (0:fibStream) (0:1:fibStream)
{-fibStream = [0,1] ++ z [0] [1]
    where
        z :: [Integer] -> [Integer] -> [Integer]
        z x y = n1 ++ z n n1 
            where 
                n = y
                n1 = zipWith (+) x y-}

--repeat' = iterate (++)

--change :: (Ord a, Num a) => a -> [[a]]
--change m = map (helper' m []) [2,3,7]
  --  where 
{- helper' m acc x 
    | m == 0 = acc
    | m < x = []
    | otherwise = (helper' (m-3) (3:acc) 3) : (helper' (m-7) (7:acc) 7) : [[]]
 -}

data Odd = Odd Integer 
    deriving (Eq, Show)

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
            next' = n' + (n' - n)
            next  = n' 

    enumFromTo (Odd n) (Odd n') 
            | n > n' = []
            | otherwise = (Odd n) : enumFromTo  (Odd (n+2)) (Odd n')

    enumFromThenTo (Odd n) (Odd next) (Odd end)    
        | n == end = [(Odd n)]
        | middle n end next = [(Odd n)] --между
        | error n next end  = []
        | otherwise             = (Odd n) : enumFromThenTo (Odd next) (Odd newNext) (Odd end)
             where 
                 newNext    = next + (next - n)
                 middle a b c = (a > b && b > c) || ( a < b && b < c)  
                 error a b c = not ((a >= b && b >= c) || (a <= b && b <= c))               
    {-
                 -- enumFromThenTo (Odd n) (Odd n') (Odd n'')
    --     | not ((n > n' && n' > n'') || (n < n' && n' < n'')) = []        
    --     | abs(n) == abs(n'')    = [(Odd n)]
    --     -- | ((n > n'' && n'' > n') || (n < n'' && n'' < n')) = [(Odd n),(Odd n'')]
        
    --     -- | next' > n''           = [(Odd n),(Odd n')]
    --     | otherwise             = (Odd n) : enumFromThenTo (Odd next) (Odd next') (Odd n'')
    --         where 
    --             next' = n' + (n' - n)
    --             next  = n'                 
    -}