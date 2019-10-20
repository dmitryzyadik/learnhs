data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle x) = pi * x ^ 2
area (Rectangle x y) = (x * y) 
data Result = Fail | Success

doSomeWork :: Int -> (Result,Int)
doSomeWork x = (Fail, x)

data Result' = Result' (Result, Int)

instance Show Result' where
    show (Result' (Success, _)) = "Success"
    show (Result' (Fail, n)) = "Fail: " ++ (show n)

doSomeWork' :: Int -> Result'
doSomeWork' x = (Result' (doSomeWork x))

data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

add :: Z -> Z -> Z
add = sum s
    where 

sum :: Bit -> Bit -> (Bit,Bit)
sum Zero Zero   = (Zero, Zero)
sum Zero One    = (One, Zero)
sum One Zero    = (One, Zero
sum One One     = (Zero, One)
            


--mul :: Z -> Z -> Z
--mul = undefined