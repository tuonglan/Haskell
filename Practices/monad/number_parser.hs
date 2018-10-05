#!/usr/bin/env stack
-- stack script --resolver lts-12.10

{-# LANGUAGE OverloadedStrings #-}


import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import qualified Data.Char as C


data Parsed = Digit Integer | Hex Integer | Word String deriving Show

-- parse the hex number
parseHexDigit :: Parsed -> Char -> [Parsed]
parseHexDigit (Hex n) c =
    if C.isHexDigit c then return $ Hex (n*16 + (toInteger (C.digitToInt c)))
                      else mzero
parseHexDigit _ _ = mzero

-- parse the digit number
parseDigit :: Parsed -> Char -> [Parsed]
parseDigit (Digit n) c =
    if C.isDigit c then return $ Digit (n*10 + (toInteger (C.digitToInt c)))
                   else mzero
parseDigit _ _ = mzero

-- parse the string
parseWord :: Parsed -> Char -> [Parsed]
parseWord (Word s) c =
    if C.isAlpha c then return $ Word (s ++ [c])
                   else mzero
parseWord _ _ = mzero

-- Parse each word and return the list of all possible outputs 
parse :: Parsed -> Char -> [Parsed]
parse p c = (parseHexDigit p c) <|> (parseDigit p c) <|> (parseWord p c)

-- Parse the entire string and retun all possible outcomes 
parseString :: String -> [Parsed]
parseString s = do
    init <- (return (Hex 0)) `mplus` (return (Digit 0)) `mplus` (return (Word ""))
    return $ putStrLn "Hello World"
    foldM parse init s


main :: IO ()
main = do
    putStrLn "Please enter a number or string:"
    s <- getLine
    putStrLn $ show (parseString s)
