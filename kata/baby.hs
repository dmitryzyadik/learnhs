{-bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny   = "You're underweight, you emo, you!"
    | bmi <= normal   = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat   = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where   bmi     = weight / height ^ 2
            skinny  = 18.5
            normal  = 25.0
            fat     = 30    
bmiTell' :: (RealFloat a) => a -> String
bmiTell' bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"

echo' :: a -> a
echo' x = x

calcBmis :: (RealFloat a ) => [(a, a)] -> [a]
calcBmis xa = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where   what [] = "empty"
            what [x] = "a singleton list."
            what xs = "a longer list."
-}
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b =
        h * (exec' f b (b-h) h)
        
        where   h = b - a / 10
                exec' f a b h                          
                    | abs (b - a) < abs h = 0
                    | otherwise = (( f a + f b) / 2) + (exec' f b (b-h) h)

integration' :: (Double -> Double) -> Double -> Double -> Double
integration' f a b =
    sum (take 1000 (exec' f a (a + h))) * h     
        where 
            exec' f x1 x2 =
                (f x1 + f x2) / 2 : exec' f x2 (x2 + h)
            h = b - a / n 
            n = 1000.0
 
integration'' :: (Double -> Double) -> Double -> Double -> [Double]
integration'' f a b =
    exec' a (a + h)
        where 
            exec' x1 x2 =
                x1 : exec' x2 (x2 + h)
            h = (b - a) / 10 
            
        -- + exec' f a b h
            {-where 
                exec' f a b h 
                    | b - a < 0.00001 = 0
                    | otherwise = f b + (f a (b-h) h) 
                h = b - a / 1000-}