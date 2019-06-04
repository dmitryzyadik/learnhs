data Person = Person String String Int Float String String deriving (Show)

data Person' = Person'  {  firstName     :: String
                        , lastName      :: String
                        , age           :: Int
                        , height        :: Float
                        , phoneNumber   :: String
                        , flavor        :: String 
                        }  deriving (Show)

data Car = Car  {  company   :: String
                , model     :: String
                , year      :: Int 
                } deriving (Show)

tellCar :: Car -> String
tellCar (Car { company = c, model =  m, year =  y}) = "Company " ++ c ++ m ++ ", year: " ++ show y                

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a  -> Vector a 
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

scalarProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `scalarProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

--let guy = Person "Fredy" "Cruger" 43 184.2 "535-434" "Escimo"
{-firstName :: Person -> String
firstName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor
-}


