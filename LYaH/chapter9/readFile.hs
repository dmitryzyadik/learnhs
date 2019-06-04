import System.IO

main = do
    --withFile "text.txt" ReadMode (\handle -> do
    --    contents <- hGetContents handle
    --    putStr contents)
    --contents <- readFile "text.txt"
    --putStr contents
    --contents <- readFile "text.txt"
    --writeFile "text2.txt" (map toUpper contents)
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")
