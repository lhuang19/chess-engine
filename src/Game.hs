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
newtype GameState a = GameState { runGameState :: StateT Game IO a }
  deriving (Functor, Applicative, Monad, MonadState Game, MonadIO)

liftIOInGame :: IO a -> GameState a
liftIOInGame = liftIO

gameLoop :: IO ()
gameLoop = evalStateT (runGameState gameLoopState) (Start startingPosition)

gameLoopState :: GameState ()
gameLoopState = do
  help
  printPos
  go

restart :: GameState ()
restart = do
  liftIOInGame $ putStrLn "Press any key to restart"
  _ <- liftIOInGame getLine
  gameLoopState

go :: GameState ()
go = do
  curr <- get
  let pos = position curr
  let colorText = show $ turn pos
  liftIOInGame $ putStrLn $ printf "Enter %s's move (e.g., 'pe2e4'):" colorText
  input <- liftIOInGame getLine
  case input of
    c
      | c `elem` [":help", ":h"] -> do
        help
        go
      | c `elem` [":undo", ":u"] -> do
        case curr of
          Start _ -> do
            liftIOInGame $ putStrLn "Cannot undo from start position."
            go
          Game _ _ prev -> do
            put prev
            printPos
            go
      | c `elem` [":fen", ":f"] -> do
            liftIOInGame $ putStrLn $ "Current FEN: " ++ FENParser.posToFEN pos
            go
      | c `elem` [":load", ":l"] -> do
            loadFEN
            go
      | otherwise -> do
          case parseMove input of
            Right move -> do
              case makeMove pos move of
                Left errMsg -> do
                  liftIOInGame $ putStrLn $ "Invalid move: " ++ errMsg
                  go
                Right newPos -> do
                  handleNewPos newPos move
            Left error -> do
              liftIOInGame $ putStrLn $ "Invalid input. Please enter a valid move. " ++ error
              go

help :: GameState ()
help = do
  liftIOInGame $ putStrLn "Commands:"
  liftIOInGame $ putStrLn ":fen :f - get current FEN"
  liftIOInGame $ putStrLn ":load :l - load a FEN"
  liftIOInGame $ putStrLn ":undo :u - undo last move"
  liftIOInGame $ putStrLn ":help :h - show this help message"

printPos :: GameState ()
printPos = do
  curr <- get
  case curr of
    Start pos -> do
      liftIOInGame $ putStrLn $ pretty pos
    Game pos _ _ -> do
      liftIOInGame $ putStrLn $ pretty pos

loadFEN :: GameState ()
loadFEN = do
  liftIOInGame $ putStrLn "Enter FEN (or q to go back):"
  fen <- liftIOInGame getLine
  case fen of
    "q" -> do
      liftIOInGame $ putStrLn "Going back..."
    _ ->
      case FENParser.parseFEN fen of
        Right pos -> do
          liftIOInGame $ putStrLn $ pretty pos
          put $ Start pos
        Left error -> do
          liftIOInGame $ putStrLn $ "Invalid FEN. Please enter a valid FEN: " ++ error
          loadFEN

handleNewPos :: Position -> Move -> GameState ()
handleNewPos newPos move = do
  liftIOInGame $ putStrLn $ pretty newPos
  curr <- get
  case curr of
    Start currPos -> do
      put $ Game newPos move curr
    Game _ _ prev -> do
      put $ Game newPos move curr
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
