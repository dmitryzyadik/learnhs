data CannotShow = CannotShow 
                deriving (Show)

data ConnotDeriveShow = ConnotDeriveShow CannotShow 
                        deriving (Show)

data OK = OK

instance Show OK where
    show _ = "OK"

data ThisWorks = ThisWorks OK 
                    deriving (Show)    

                    