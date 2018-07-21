{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Person

getJsonFile :: FilePath -> IO B.ByteString
getJsonFile file = B.readFile file

getJsonUrl :: String -> IO B.ByteString
getJsonUrl url = simpleHttp url


main :: IO ()
main = do
    ret <- (eitherDecode <$> (getJsonUrl "http://daniel-diaz.github.io/misc/pizza.json")
           ) :: IO (Either String [Person])
    case ret of
        Left err -> putStrLn err
        Right j -> print j

