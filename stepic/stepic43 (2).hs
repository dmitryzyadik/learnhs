import Data.Time.Clock
import Data.Time.Format
--import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving (Show)

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String} deriving (Show)

logLevelToString :: LogLevel -> String
logLevelToString = show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Show)

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p1 {lastName = p2Name}
    where p2Name = lastName p2

data Shape = Circle Double | Rectangle Double Double

isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False

--Определить функцию abbrFirstName, которая сокращает имя до первой буквы с точкой, то есть, если имя было "Ivan", 
--то после применения этой функции оно превратится в "I.". Однако, если имя было короче двух символов, то оно не меняется.

--data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p = hf $ firstName p
    where               
        hf fn 
            | length fn > 2 = (head fn) ++ "."
            | otherwise = "12"