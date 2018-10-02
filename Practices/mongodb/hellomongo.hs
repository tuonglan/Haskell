#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

promptLine :: String -> IO String
promptLine msg = do
    putStrLn msg
    getLine

main :: IO ()
main = do
    text1 <- promptLine "Enter the first text:"
    text2 <- promptLine "Enter the second text:"
    putStrLn $ "You entered: " ++ text1 ++ " " ++ text2
