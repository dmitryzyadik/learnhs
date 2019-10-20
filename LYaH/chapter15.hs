data T a = Empt | N a (T a) (T a) deriving (Show)
data D = L | R deriving (Show)
type Ds = [D]

freeTree :: T Char
freeTree =   
    N 'P'  
        (N 'O'  
            (N 'L'  
                (N 'N' Empt Empt)  
                (N 'T' Empt Empt)  
            )  
            (N 'Y'  
                (N 'S' Empt Empt)  
                (N 'A' Empt Empt)  
            )  
        )  
        (N 'L'  
            (N 'W'  
                (N 'C' Empt Empt)  
                (N 'R' Empt Empt)  
            )  
            (N 'A'  
                (N 'A' Empt Empt)  
                (N 'C' Empt Empt)  
            )  
        )  

changeToP :: Ds -> T Char -> T Char
changeToP (L:ds) (Node x l r) = N x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = N x l (changeToP ds r)
changeToP [] (N _ l r) = N 'P' l r

