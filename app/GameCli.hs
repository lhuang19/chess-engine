module Main where

import Game
import Syntax
import Util

main :: IO ()
main = do
  putStrLn "Welcome to Haskell Chess!"
  gameLoop
