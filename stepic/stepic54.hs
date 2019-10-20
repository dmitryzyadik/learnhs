import Data.Char

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken x
    | isD x = Just (Number (read x :: Int))
    | x == "+"  = Just (Plus)
    | x == "-"  = Just (Minus)
    | x == "("  = Just (LeftBrace)
    | x == ")"  = Just (RightBrace)
    | otherwise = Nothing
    where
        isD = foldl (\x y -> Data.Char.isDigit y && x ) True 

tokenize :: String -> Maybe [Token]
tokenize input  = mapM (asToken) (words input)  