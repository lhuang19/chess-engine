module Main where

import Syntax
import Game

main :: IO ()
main = do
  putStrLn "Welcome to Haskell Chess!"
  gameLoop

