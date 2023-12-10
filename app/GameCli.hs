module Main where

import System.Environment (getArgs)

import Game
import Syntax
import Util
import FENParser

main :: IO ()
main = do
  putStrLn "Welcome to Haskell Chess!"
  args <- getArgs
  putStrLn $ "Args: " ++ show args
  case unwords args of
    [] -> gameLoop (Start startingPosition (GameStateOptions False False))
    args -> do
      case FENParser.parseFEN args of
        Left err -> putStrLn err
        Right pos -> gameLoop (Start pos (GameStateOptions False False))
