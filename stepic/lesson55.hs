{-main :: IO ()
main = do
    putStr "What is your name?"
    putStr "Name: "
    name <- getLine
    if name /= ""
        then do
            putStrLn $ "Hi, " ++ name ++ "!"
        else
            main-}

import System.Directory
import Data.List

main = do
    putStr "Substring: "    
    subString <- getLine
    if null subString 
        then putStrLn "Canceled"
        else do
            dirContent <- getDirectoryContents "."
            let files = filter (isInfixOf subString) $ filter isRegularFile dirContent         
            mapM_ deleteFile files
            --print files
    --return ()
deleteFile :: FilePath -> IO ()
deleteFile filePath = do
    putStr "Removing file: " 
    putStrLn filePath
     

isRegularFile :: FilePath -> Bool    
isRegularFile f = isInfixOf "." f
            
            
    