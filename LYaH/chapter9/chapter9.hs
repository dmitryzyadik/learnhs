Import Control.Monad
Import Data.Char

main = forever $ do
    l <- getLine
    ptStrLn S map toUpper l