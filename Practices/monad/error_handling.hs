#!/usr/bin/env stack
-- stack script --resolver lts-12.10

{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Exception as E
import Data.Typeable
import Control.Monad.Except


-- Catching error using Control.Exception.catch
myDiv1 :: Float -> Float -> Float
myDiv1 x 0 = error "Division by zero"
myDiv1 x y = x/y

example1 :: Float -> Float -> IO ()
example1 x y =
    E.catch (putStrLn (show $ myDiv1 x y))
            (\(E.ErrorCall err) -> putStrLn $ show err)

-- Catching erro using Maybe
myDiv2 :: Float -> Float -> Maybe Float
myDiv2 x 0 = Nothing
myDiv2 x y = Just (x/y)

example2 :: Float -> Float -> IO ()
example2 x y =
    case myDiv2 x y of
        Nothing -> putStrLn "Division by zero"
        Just a -> putStrLn (show a)

-- Catching error using Either
myDiv3 :: Float -> Float -> Either String Float
myDiv3 x 0 = Left "Division by zero"
myDiv3 x y = Right $ x/y

example3 :: Float -> Float -> IO ()
example3 x y =
    case myDiv3 x y of
        Left s -> putStrLn s
        Right q -> putStrLn (show q)

-- Use Monad and fail to catch error
myDiv4 :: (Monad m) => Float -> Float -> m Float
myDiv4 x 0 = fail "Division by zero, raised by 'fail'"
myDiv4 x y = return (x / y)

example4a :: Float -> Float -> IO ()
example4a x y =
    case myDiv4 x y of
        Nothing -> putStrLn "Division by zeor (example 4a)"
        Just q -> putStrLn $ show q

example4b :: Float -> Float -> IO ()
example4b x y =
    case myDiv4 x y of
        Left msg -> putStrLn $ msg ++ " (example 4b)"
        Right q  -> putStrLn $ show q

example4c :: Float -> Float -> IO ()
example4c x y =
    E.catch (do q <- myDiv4 x y; putStrLn $ show q)
            (\(E.ErrorCall err) -> putStrLn $ show err)

-- Go nuts with monad transformer
data CustomError = DivByZero | OutOfCheese | MiscError String

instance Show CustomError where
    show DivByZero = "Division by zero (custom error)"
    show OutOfCheese = "Out of cheese (custom error)"
    show (MiscError s) = s

type DivIO = ExceptT CustomError IO

myDiv8 :: Float -> Float -> DivIO Float
myDiv8 x 0 = throwError DivByZero
myDiv8 x y = return (x/y)

example8 :: Float -> Float -> IO ()
example8 x y = do
    r <- runExceptT $ myDiv8 x y
    case r of
        Left msg -> putStrLn $ show msg
        Right num -> putStrLn $ show num

main :: IO ()
main = do
    example1 1 2
    example1 1 0
    example2 1 3
    example2 2 0
    example3 1 4
    example3 3 0
    example4a 1 5
    example4a 4 0
    --example4c 1 6
    --example4c 5 0
    --example4b 1 7
    --example4b 6 0
    example8 7 0
    putStrLn "Finish the error_handling"


