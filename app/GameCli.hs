module Main where

import Game
import Syntax

main :: IO ()
main = do
  putStrLn "Welcome to Haskell Chess!"
  gameLoop
