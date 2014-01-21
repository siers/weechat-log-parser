import Control.Applicative
import Data.Attoparsec.Text
import Data.Time
import Data.Text as T hiding (count, head)
import Data.Text.IO as TI (readFile)
import Data.Char (isSpace)
import System.Environment (getArgs)

data LogLine = LogLine { date :: LocalTime
                       , nick :: Text
                       , message :: Text
                       } deriving Show

parseLog :: Parser [LogLine]
parseLog = many1 $ parseLine <* endOfLine

parseLine :: Parser LogLine
parseLine = do
    logDate <- parseDate
    char '\t'
    logNick <- takeTill isSpace
    char '\t'
    logMessage <- takeTill isEndOfLine
    return $ LogLine logDate logNick logMessage


parseDate :: Parser LocalTime
parseDate = do
    year <- count 4 digit
    char '-'
    month <- count 2 digit
    char '-'
    day <- count 2 digit
    char ' '
    hour <- count 2 digit
    char ':'
    minute <- count 2 digit
    char ':'
    second <- count 2 digit
    return LocalTime { localDay = fromGregorian (read year) (read month) (read day)
                     , localTimeOfDay = TimeOfDay (read hour) (read minute) (read second)
                     }

printLog :: Either String [LogLine] -> IO ()
printLog p = case p of
    Left x -> putStrLn $ "Error: " ++ x
    Right x -> putStrLn $ assembleLine x

assembleLine :: [LogLine] -> String
assembleLine (x:xs) = (show $ date x) ++ " <" ++ (T.unpack $ nick x) ++ "> " ++ (T.unpack $ message x) ++ "\n" ++ assembleLine xs
assembleLine [] = ""

main :: IO ()
main = do
    args <- getArgs
    TI.readFile (head args) >>= printLog . parseOnly parseLog