#!/usr/bin/env stack
-- stack script --resolver lts-8.22

{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Attoparsec.Char8
import Data.Word
import Data.Time
import qualified Data.String as S
import qualified Data.ByteString as B

--- IP parser ---
data IP = IP Word8 Word8 Word8 Word deriving Show

ipParser :: Parser IP
ipParser = do
    d1 <- decimal; char '.'
    d2 <- decimal; char '.'
    d3 <- decimal; char '.'
    d4 <- decimal
    return $ IP d1 d2 d3 d4

--- Date time parser ---
timeParser :: Parser LocalTime
timeParser = do
    year <- count 4 digit; char '-'
    month <- count 2 digit; char '-'
    day <- count 2 digit; char ' '
    hour <- count 2 digit; char ':'
    min <- count 2 digit; char ':'
    sec <- count 2 digit
    return $ LocalTime { localDay = fromGregorian (read year) (read month) (read day)
                       , localTimeOfDay = TimeOfDay (read hour) (read min) (read sec)
                       }

-- Product parser --
data Product = Keyboard | Mouse | Monitor | Speaker deriving Show

productParser :: Parser Product
productParser = do
    (string "Keyboard" >> return Keyboard)
    <|> (string "Mouse" >> return Mouse)
    <|> (string "Monitor" >> return Monitor)
    <|> (string "Speaker" >> return Speaker)

--- Source parser ---
data Source = Internet | Friend | NoAnswer deriving Show

sourceParser :: Parser Source
sourceParser = do
    (string "Internet" >> return Internet)
    <|> (string "Friend" >> return Friend)
    <|> (string "NoAnswer" >> return NoAnswer)

--- LogEntry parser ---
data LogEntry = LogEntry
    { entryTime :: LocalTime
    , entryIP :: IP
    , entryProduct :: Product
    , entrySource :: Source
    } deriving Show

entryParser :: Parser LogEntry
entryParser = do
    time <- timeParser; char ','
    ip <- ipParser; char ','
    product <- productParser; char ','
    source <- sourceParser
    return $ LogEntry time ip product source

--- Log Parser ---
type Log = [LogEntry]
logParser :: Parser Log
logParser = many $ entryParser <* endOfLine


--- Main ---
csvFile :: FilePath
csvFile = "selling.csv"

main = do
    putStrLn "Hello World"
    log <- fmap (parseOnly logParser) $ B.readFile csvFile
    putStrLn $ show log

