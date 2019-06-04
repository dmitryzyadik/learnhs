data Log a = Log [String] a deriving (Show)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f s x = Log [s] (f x) 

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (s++s1) a1  
    where
        Log s a = f x
        Log s1 a1 = g a        

returnLog :: a -> Log a
returnLog = \x -> (Log [] x)      

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log s n) f = Log (s++s1) n1  
    where
        Log s1 n1 = f n 

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (bindLog) . (returnLog)

data SomeType a = SomeType a

instance Functor SomeType  where
    fmap f x = (>>=) . (return x) f 