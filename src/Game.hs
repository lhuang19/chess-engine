module Game (gameLoop) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State
import FENParser
import MoveParser
import Moves
import Print
import Syntax
import Util

gameLoop :: IO ()
gameLoop = go' startingPosition
  where
    go' :: Position -> IO ()
    go' pos = do
      putStrLn "Type FEN at any time to get current FEN"
      putStrLn $ pretty pos
      go pos

    go :: Position -> IO ()
    go pos = do
      putStrLn "Enter your move (e.g., 'pe2e4'):"
      input <- getLine
      if input == "FEN"
        then do
          putStrLn $ "Current FEN: " ++ FENParser.posToFEN pos
          go pos
        else case parseMove input of
          Right move -> do
            case makeMove pos move of
              Left errMsg -> do
                putStrLn $ "Invalid move: " ++ errMsg
                go pos
              Right newPos -> do
                putStrLn $ "New Position: " ++ pretty newPos
                putStrLn $ pretty newPos
                go newPos
          Left error -> do
            putStrLn $ "Invalid input. Please enter a valid move." ++ error
            go pos
