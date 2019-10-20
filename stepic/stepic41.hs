data Color = Red | Green | Blue

instance Show Color where
    show Red    = "Red"
    show Green  = "Green"
    show Blue   = "Blue"


charToInt :: Char -> Int
charToInt '0' = 0
charToInt  '1' = 1
charToInt  '2' = 2
charToInt  '3' = 3
charToInt  '4' = 4
charToInt  '5' = 5
charToInt  '6' = 6
charToInt  '7' = 7
charToInt  '8' = 8
charToInt '9' = 9

emptyOrSingleton :: Bool -> a -> [a]
emptyOrSingleton False _ = []
emptyOrSingleton True x = []

isEqual :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
isEqual (a, b) (a', b') = True

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Warning   = GT
cmp Error Info      = GT
cmp Info Warning    = LT
cmp Info Error      = LT
cmp Warning Info    = GT
cmp Warning Error   = LT
cmp _ _ = EQ
