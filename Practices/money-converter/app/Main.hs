{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import ConvertTypes
import Network.HTTP.Conduit (simpleHttp)
import Data.Text
import Data.Aeson

tokenKey :: String
tokenKey = "07e12ba917b15e7d3911e2a369ba9ed2"

getConversion :: String -> String -> Double -> IO (Maybe Convert)
getConversion from to amount =
    fmap decode $ simpleHttp $ "http://data.fixer.io/api/convert?access_key="
                               ++ tokenKey ++ "&from=" ++ from ++ "&to=" ++ to
                               ++ "&amount=" ++ (show amount)


main :: IO ()
main = do
    let amount = 200
    conv <- getConversion "USD" "KRW" amount
    case conv of
        Nothing -> putStrLn "Error when converting from USD to KRW"
        Just v -> putStrLn $ "Result: " ++ (show amount) ++ " USD equivalent to "
                             ++ (show $ result v) ++ " KRW"
