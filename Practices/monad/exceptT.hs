#!/usr/bin/env stack
-- stack script --resolver lts-12.10

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except


type LengthMonad = ExceptT String IO

calculateLength :: LengthMonad Int
calculateLength = do
    liftIO $ putStrLn "Please enter a non-empty string:"
    s <- liftIO getLine
    if null s then throwError "The string was empty!"
              else return $ length s

reportResult :: Either String Int -> IO ()
reportResult (Left e) = putStrLn $ "Length calculation error with message: " ++ (show e)
reportResult (Right len) = putStrLn $ "The length of the string is " ++ (show len)


main :: IO ()
main = do
    r <- runExceptT calculateLength
    reportResult r
