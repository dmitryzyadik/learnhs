import Data.Char(toUpper)

isGreen :: IO Bool
isGreen = 
    do putStrLn "Is green you favorite color?"
       inpStr <- getLine
       return ((toUpper . head $ inpStr) == 'Y')