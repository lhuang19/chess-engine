module Main where

import Print
import Syntax

main :: IO ()
main = do
  putStrLn "Hello CIS 5520"
  putStrLn "Testing Syntax"
  putStrLn $ pretty wBoard1
  putStrLn $ pretty wPosition1
  putStrLn "Testing Syntax"
