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

-- Define a newtype for your state
newtype GameState a = GameState { runGameState :: StateT Position IO a }
  deriving (Functor, Applicative, Monad, MonadState Position, MonadIO)

-- Define a custom lift function to lift IO actions into the GameState monad
liftIOInGame :: IO a -> GameState a
liftIOInGame = liftIO

-- Modify the gameLoop function to use the StateT monad
gameLoop :: IO ()
gameLoop = evalStateT (runGameState gameLoopState) startingPosition

-- Define the actual gameLoop logic in the StateT monad
gameLoopState :: GameState ()
gameLoopState = do
  liftIOInGame $ putStrLn "Type FEN at any time to get current FEN"
  liftIOInGame $ putStrLn "Type LOAD to load a FEN"
  liftIOInGame $ putStrLn $ pretty startingPosition
  go

restart :: GameState ()
restart = do
  liftIOInGame $ putStrLn "Press any key to restart"
  _ <- liftIOInGame getLine
  gameLoopState

go :: GameState ()
go = do
  pos <- get
  let colorText = show $ turn pos
  liftIOInGame $ putStrLn $ printf "Enter %s's move (e.g., 'pe2e4'):" colorText
  input <- liftIOInGame getLine
  if input == "FEN"
    then do
      liftIOInGame $ putStrLn $ "Current FEN: " ++ FENParser.posToFEN pos
      go
    else
      if input == "LOAD"
        then do
          fen <- promptForFEN
          handleNewPos fen
        else case parseMove input of
          Right move -> do
            case makeMove pos move of
              Left errMsg -> do
                liftIOInGame $ putStrLn $ "Invalid move: " ++ errMsg
                go
              Right newPos -> do
                handleNewPos newPos
          Left error -> do
            liftIOInGame $ putStrLn $ "Invalid input. Please enter a valid move. " ++ error
            go

promptForFEN :: GameState Position
promptForFEN = do
  liftIOInGame $ putStrLn "Enter FEN:"
  fen <- liftIOInGame getLine
  case FENParser.parseFEN fen of
    Right pos -> return pos
    Left error -> do
      liftIOInGame $ putStrLn $ "Invalid FEN. Please enter a valid FEN: " ++ error
      promptForFEN

handleNewPos :: Position -> GameState ()
handleNewPos newPos = do
  liftIOInGame $ putStrLn $ pretty newPos
  put newPos
  case gameCondition newPos of
    Checkmate -> do
      liftIOInGame $ putStrLn $ printf "Checkmate! %s wins." (show $ flipColor $ turn newPos)
      restart
    Stalemate -> do
      liftIOInGame $ putStrLn "Stalemate!"
      restart
    FiftyMoveDraw -> do
      liftIOInGame $ putStrLn "Draw by fifty-move rule."
      restart
    Check -> go
    Normal -> go
