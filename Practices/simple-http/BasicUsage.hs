#!/usr/bin/env stack
-- stack script --resolver lts-8.22

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Simple
import Data.Aeson
import Data.Yaml as Yaml

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"
    putStrLn $ "The response code is: " ++ show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L.putStrLn $ getResponseBody response

    putStrLn "\n"
    r2 <- httpJSON "http://httpbin.org/get"
    C8.putStrLn $ Yaml.encode (getResponseBody r2 :: Value)
