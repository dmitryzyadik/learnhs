import Data.Time.Clock
import Data.Time.Format
--import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving (Show)

data LogEntry = LogEntry {timestamp  :: UTCTime, logLevel  :: LogLevel, message :: String} deriving (Show)  


logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString (LogEntry {timestamp = t, logLevel = l,  message = m}) 
    =  (timeToString t) ++ ": " ++ (logLevelToString l) ++ ": " ++ m


data Person = Person { firstName :: String, lastName :: String, age :: Int }

updateLastName :: Person -> Person -> Person
updateLastName p1 p2  = p2 {lastName = (lastName p1)}

abbrFirstName :: Person -> Person
abbrFirstName p = p {firstName = (hfn fn)}
    where 
        fn = firstName p
        hfn :: [Char] -> [Char]
        hfn fn 
            | (length fn) > 2   = (head fn) : "." 
            | otherwise         = fn  
