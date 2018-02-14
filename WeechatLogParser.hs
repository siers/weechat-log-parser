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
    (y, mo, d) <- (,,) <$> dsep 4 '-' <*> dsep 2 '-' <*> dsep 2 ' '
    (h, mi, s) <- (,,) <$> dsep 2 ':' <*> dsep 2 ':' <*> count 2 digit

    return LocalTime
        { localDay = fromGregorian (read y) (read mo) (read d)
        , localTimeOfDay = TimeOfDay (read h) (read mi) (read s) }

    where dsep n c = count n digit <* char c

main = mapM_ print . either error id . parseOnly parseLog =<< TI.getContents
