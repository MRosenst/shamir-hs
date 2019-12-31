module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Enter code:"
    code <- readLn
    putStrLn "Enter threshold:"
    threshold <- readLn
    putStrLn "Enter number of code fragments:"
    n <- readLn
    fragments <- splitCode code threshold 
    print $ take n fragments