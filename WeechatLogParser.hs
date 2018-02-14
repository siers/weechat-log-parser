{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Maybe
import Data.Text as T hiding (count, head)
import Data.Text.IO as TI (getContents)
import Data.Time
import qualified Data.List as List (find)
import System.Environment (getArgs)

data LogLine =
    LogMsg { logTime :: LocalTime, user :: Text, msg :: Text }
    | LogJoin { logTime :: LocalTime, msg :: Text }
    | LogPart { logTime :: LocalTime, msg :: Text }
    deriving Show

parseLog :: Parser [LogLine]
parseLog = many1 $ parseLine <* endOfLine

parseLine :: Parser LogLine
parseLine = do
    date <- parseDate <* char '\t'
    userOrAction <- takeTill isSpace <* char '\t'

    log <- return . snd . fromJust . List.find fst $
        [ (userOrAction == "-->", LogJoin)
        , (userOrAction == "<--", LogPart)
        , (True, flip LogMsg userOrAction) ]

    log date <$> takeTill isEndOfLine

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
