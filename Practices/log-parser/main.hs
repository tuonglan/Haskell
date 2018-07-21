#!/usr/bin/env stack
-- stack script --resolver lts-8.22

{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Char8
import Control.Applicative
import Data.Word
import Data.Time
import qualified Data.ByteString as B

data IP = IP Word8 Word8 Word8 Word8 deriving Show

ipParser :: Parser IP
ipParser = do
    d1 <- decimal
    char '.'
    d2 <- decimal
    char '.'
    d3 <- decimal
    char '.'
    d4 <- decimal
    return $ IP d1 d2 d3 d4

data Product = Mouse | Keyboard | Monitor | Speaker deriving Show

data LogEntry = LogEntry
    { entryTime :: LocalTime
    , entryIP :: IP
    , entryProduct :: Product
    , entrySource :: Source
    } deriving Show

type Log = [LogEntry]

-- Time parser format: yyyy-MM-dd hh:mm:ss
timeParser :: Parser LocalTime
timeParser = do
    y <- count 4 digit
    char '-'
    mm <- count 2 digit
    char '-'
    d <- count 2 digit
    char ' '
    h <- count 2 digit
    char ':'
    m <- count 2 digit
    char ':'
    s <- count 2 digit
    return $ LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
                       , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
                       }


-- Source parser
data Source = Internet | Friend | NoAnswer deriving Show
sourceParser :: Parser Source
sourceParser = do
    (string "internet" >> return Internet)
    <|> (string "friend" >> return Friend)
    <|> (string "noanswer" >> return NoAnswer)


-- Product parser
productParser :: Parser Product
productParser = 
    (string "mouse" >> return Mouse)
    <|> (string "keyboard" >> return Keyboard)
    <|> (string "monitor" >> return Monitor)
    <|> (string "speakers" >> return Speaker)

-- LogEntry parser
logEntryParser :: Parser LogEntry
logEntryParser = do
    d <- timeParser
    char ' '
    ip <- ipParser
    char ' '
    p <- productParser
    opt <- option NoAnswer $ char ' ' >> sourceParser
    return $ LogEntry d ip p opt

-- Log parser
logParser :: Parser Log
logParser = many $ logEntryParser <* endOfLine


-------------------------------
logFile :: FilePath
logFile = "selling.log"


main :: IO ()
main = do
    B.readFile logFile >>= print . parseOnly logParser
