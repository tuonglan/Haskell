#!/usr/bin/env stack
-- stack script --resolver lts-12.10

{-# LANGUAGE OverloadedStrings #-}

import System.Random
import Control.Applicative
import Control.Monad.Trans.State


-- Roll the dice with IO
rollDiceIO :: IO (Int, Int)
rollDiceIO = liftA2 (,) (randomRIO (1,6)) (randomRIO (1,6))

mainDiceIO :: IO ()
mainDiceIO = do
    dice <- rollDiceIO
    putStrLn $ "The dice is: " ++ (show dice)

-----------------------------------------------
--- Roll the dice without IO ---
rollDice :: State StdGen Int
rollDice = state $ randomR (1, 6)

mainDice :: IO ()
mainDice = do
    dice <- return $ evalState rollDice (mkStdGen 1)
    putStrLn $ "The dice is: " ++ (show dice)

main :: IO ()
main = do
    mainDice
