import Data.Char(isDigit)
data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2)=  sqrt $ (x2-x1)^2 + (y2-y1)^2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2)=  abs (x2-x1) + abs (y2-y1)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (h:t) | isDigit h = Just h
                | otherwise = findDigit t

findDigitOrX :: [Char] -> Char
findDigitOrX x = 
        case findDigit x of
            Nothing -> 'X'
            Just xs -> xs

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = x : []

listToMaybe :: [a] -> Maybe a
listToMaybe []  = Nothing
listToMaybe (x:_) = Just x


data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Show, Read)
{-Реализуйте функцию parsePerson, которая разбирает строки вида 
firstName = John\nlastName = Connor\nage = 30 и возвращает либо результат типа Person, либо ошибку типа Error.

Строка, которая подается на вход, должна разбивать по символу '\n' на список строк, каждая из которых имеет вид X = Y. 
Если входная строка не имеет указанный вид, то функция должна возвращать ParsingError.
Если указаны не все поля, то возвращается IncompleteDataError.
Если в поле age указано не число, то возвращается IncorrectDataError str, где str — содержимое поля age.
Если в строке присутствуют лишние поля, то они игнорируются.-}

parsePerson :: String -> Person
parsePerson s =   
            where 
                l = lines s 
                p x = () 

parseValue :: String -> (String,String)

parseValue []        = ("","")           
parseValue (x1:x2:x3:y) = ("",y)
parseValue y         = parseValue $ tail y