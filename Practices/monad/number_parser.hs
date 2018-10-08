#!/usr/bin/env stack
-- stack script --resolver lts-12.10

{-# LANGUAGE OverloadedStrings #-}


import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
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
parse p c = (parseHexDigit p c) `mplus` (parseDigit p c) `mplus` (parseWord p c)

-- Init the very first elements
initParsed :: [Parsed]
initParsed = do
    (return (Hex 0)) `mplus` (return (Digit 0)) `mplus` (return (Word ""))

foldParseds :: Parsed -> String -> [Parsed]
foldParseds init s = do
    foldM parse init s

-- Parse the entire string and retun all possible outcomes 
parseStringMaybe :: String -> MaybeT IO [Parsed]
parseStringMaybe s = do
    init <- lift $ (return (Hex 0)) `mplus` (return (Digit 0)) `mplus` (return (Word ""))
    lift $ putStrLn ("The init is: " ++ (show init))
    return $ foldM parse init s

parseString :: String -> IO [Parsed]
parseString s = do
    (init:_) <- return $ initParsed
    putStrLn $ "The init is: " ++ (show init)
    return $ foldParseds init s


main :: IO ()
main = do
    putStrLn "Please enter a number or string:"
    s <- getLine
    result <- parseString s
    putStrLn $ show result


