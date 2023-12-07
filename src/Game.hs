module Game (gameLoop) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State
import FENParser
import MoveParser
import Moves
import Print
import Syntax
import Text.Printf (printf)
import Util

gameLoop :: IO ()
gameLoop = restart
  where
    restart :: IO ()
    restart = do
      putStrLn "Type FEN at any time to get current FEN"
      putStrLn "Type LOAD to load a FEN"
      putStrLn $ pretty startingPosition
      go startingPosition

    go :: Position -> IO ()
    go pos = do
      let colorText = show $ turn pos
      putStrLn $ printf "Enter %s's move (e.g., 'pe2e4'):" colorText
      input <- getLine
      if input == "FEN"
        then do
          putStrLn $ "Current FEN: " ++ FENParser.posToFEN pos
          go pos
        else
          if input == "LOAD"
            then do
              fen <- promptForFEN
              handleNewPos fen
            else case parseMove input of
              Right move -> do
                case makeMove pos move of
                  Left errMsg -> do
                    putStrLn $ "Invalid move: " ++ errMsg
                    go pos
                  Right newPos -> do
                    handleNewPos newPos
              Left error -> do
                putStrLn $ "Invalid input. Please enter a valid move. " ++ error
                go pos

    promptForFEN :: IO Position
    promptForFEN = do
      putStrLn "Enter FEN:"
      fen <- getLine
      case FENParser.parseFEN fen of
        Right pos -> do
          return pos
        Left error -> do
          putStrLn $ "Invalid FEN. Please enter a valid FEN: " ++ error
          promptForFEN

    handleNewPos :: Position -> IO ()
    handleNewPos newPos = do
      putStrLn $ pretty newPos
      case gameCondition newPos of
        Checkmate -> do
          putStrLn $ printf "Checkmate! %s wins." (show $ flipColor $ turn newPos)
          putStrLn "Press any key to restart"
          _ <- getLine
          restart
        Stalemate -> do
          putStrLn "Stalemate!"
          putStrLn "Press any key to restart"
          _ <- getLine
          restart
        FiftyMoveDraw -> do
          putStrLn "Draw by fifty move rule."
          putStrLn "Press any key to restart"
          _ <- getLine
          restart
        Check -> do
          putStrLn "Check!"
          go newPos
        Normal -> do
          go newPos
