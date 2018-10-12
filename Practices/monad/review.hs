#!/usr/bin/env stack
-- stack script --resolver lts-12.10

{-# LANGUAGE OverloadedStrings #-}


doList :: [Int]
doList = do
    x <- [10, 20, 30]
    y <- [x, x+1]
    if y > 20 then [] else [y, y]

main :: IO ()
main = do
    putStrLn "Hello Review"
    ls <- return doList
    putStrLn $ show ls
    
