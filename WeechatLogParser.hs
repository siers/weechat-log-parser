{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Text as T hiding (count, head)
import Data.Text.IO as TI (getContents)
import Data.Time
import System.Environment (getArgs)

data LogLine = LogLine LocalTime Text Text

instance Show LogLine where
    show (LogLine a b c) = show a ++ " <" ++ T.unpack b ++ "> " ++ T.unpack c

parseLog :: Parser [LogLine]
parseLog = many1 $ parseLine <* endOfLine

parseLine :: Parser LogLine
parseLine = do
    LogLine
        <$> parseDate <* char '\t'
        <*> takeTill isSpace <* char '\t'
        <*> takeTill isEndOfLine

parseDate :: Parser LocalTime
parseDate = do
    (year, month, day) <- (,,) <$>
        count 4 digit <* char '-' <*>
        count 2 digit <* char '-' <*>
        count 2 digit

    char ' '

    (hour, minute, second) <- (,,) <$>
        count 2 digit <* char ':' <*>
        count 2 digit <* char ':' <*>
        count 2 digit

    return LocalTime { localDay = fromGregorian (read year) (read month) (read day)
                     , localTimeOfDay = TimeOfDay (read hour) (read minute) (read second)
                     }

main = mapM_ print . either error id . parseOnly parseLog =<< TI.getContents
