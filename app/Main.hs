module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Hello CIS 5520"
  putStrLn "Testing Syntax"
  putStrLn someFunc
  putStrLn $ pretty wBoard1
