#!/usr/bin/usr stack
-- stack script --resolver lts-12.10

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except


-- Length error type
data LengthError = EmptyString 
            | StringTooLong Int 
            | OtherError String
instance Show LengthError where
    show EmptyString = "The string was empty!"
    show (StringTooLong len) = "The length of the string (" ++ (show len) ++ ") is bigger than 5!"
    show (OtherError msg) = msg
type LengthMonad = Either LengthError

-- Calculate the length and raise the excepiton if needed
calculateLength :: String -> LengthMonad Int
calculateLength [] = throwError EmptyString
calculateLength s
    | len > 5 = throwError $ StringTooLong len
    | otherwise = return len
  where len = length s

-- Report the lengh if there's no error
reportResult :: LengthMonad Int -> IO ()
reportResult (Right len) = putStrLn $ "The length of the string is " ++ (show len)
reportResult (Left msg) = putStrLn $ "Length calculation failed with message: " ++ (show msg)


main :: IO ()
main = do
    putStrLn "Please enter a string"
    s <- getLine
    reportResult $ calculateLength s

