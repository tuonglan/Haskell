#!/usr/bin/env stack
-- stack script --resolver lts-8.22

{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Char8
import Control.Applicative
import Data.Word
import Data.Time
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Maybe as Maybe

import Data.String as S
import Data.Char (toLower)
import qualified Data.Monoid as Monoid
import qualified Data.Foldable as Foldable

import qualified Merge as M

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

data Product = Mouse | Keyboard | Monitor | Speaker deriving (Show, Eq)

data LogEntry = LogEntry
    { entryTime :: LocalTime
    , entryIP :: IP
    , entryProduct :: Product
    , entrySource :: Source
    } deriving Show

instance Eq LogEntry where
    le1 == le2 = entryTime le1 == entryTime le2
instance Ord LogEntry where
    le1 >= le2 = entryTime le1 >= entryTime le2
    le1 <= le2 = entryTime le1 <= entryTime le2

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


---- Extracting information ---
type Sales = [(Product, Int)]

saleOf :: Product -> Sales -> Int
saleOf p sales = Maybe.fromMaybe 0 $ lookup p sales

addSale :: Product -> Sales -> Sales
addSale p [] = [(p, 1)]
addSale p ((x, n):xs)
    | p == x = (x, n+1):xs
    | otherwise = (x, n) : (addSale p xs)

maxBy :: Ordering -> a -> a -> a
maxBy o a b = case o of
    LT -> b
    EQ -> a
    GT -> a

mostSold :: Sales -> Maybe (Product, Int)
mostSold [] = Nothing
mostSold xs = Just $ foldr1 (\p1 p2 -> maxBy (compare (snd p1) (snd p2)) p1 p2) xs

sales :: Log -> Sales
sales = foldr (addSale . entryProduct) []

---- Rendering data ----
sepChar :: Char
sepChar = ','

renderIP :: IP -> B.ByteString
renderIP (IP a b c d) = 
    S.fromString $ (show a) ++ "." ++ (show b) ++ "." ++ (show c) ++ "." ++ (show d)

renderEntry :: LogEntry -> B.ByteString
renderEntry (LogEntry time ip product source) = 
    S.fromString $ (show time) ++ "," ++ (C8.unpack $ renderIP ip) ++ "," ++ (show product) ++ 
                   "," ++ (show source)

renderLog :: Log -> B.ByteString
renderLog [] = S.fromString ""
renderLog (x:xs) = B.concat $ [renderEntry x, S.fromString "\n", renderLog xs]

-------------------------------
logFile :: FilePath
logFile = "selling.log"
logFile2 :: FilePath
logFile2 = "selling2.log"

csvFile :: FilePath
csvFile = "selling.csv"

main :: IO ()
main = do
    logs1 <- fmap (parseOnly logParser) $ B.readFile logFile
    logs2 <- fmap (parseOnly logParser) $ B.readFile logFile2
    let logs = eitherMerge logs1 logs2
          where eitherMerge (Right l1) (Right l2) = M.merge l1 l2
                eitherMerge (Left err) _ = [] --printError $ "A parsing error was found: " ++ err
                eitherMerge _ (Left err) = [] --printError $ "A parsing error was found: " ++ err

    print $ sales logs
    print "The most sale is: "
    print $ mostSold $ sales logs
    print "List of products in CSV:"
    -- mapM_ (print . renderEntry) logs
    B.putStrLn $ renderLog logs
    print "Writing logs to csv file..."
    B.writeFile csvFile $ renderLog logs
