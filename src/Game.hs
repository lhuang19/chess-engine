module Game (gameLoop) where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State
import FENParser
import MoveParser
import Moves
import Print
import Syntax
import Text.Printf (printf)
import Util
import Engine

-- Define a newtype for your state
newtype GameState a = GameState { runGameState :: StateT Game IO a }
  deriving (Functor, Applicative, Monad, MonadState Game, MonadIO)

liftIOInGame :: IO a -> GameState a
liftIOInGame = liftIO

gameLoop :: Game -> IO ()
gameLoop = evalStateT (runGameState gameLoopState) 

gameLoopState :: GameState ()
gameLoopState = do
  help
  printPos
  go

restart :: GameState ()
restart = do
  liftIOInGame $ putStrLn "Press any key to restart"
  _ <- liftIOInGame getLine
  liftIOInGame $ putStrLn "Restarting..."
  let gameOptions = GameStateOptions { aiModeWhite = False, aiModeBlack = False }
  put $ Start { position = startingPosition, options = gameOptions }
  gameLoopState

go :: GameState ()
go = do
  curr <- get
  let pos = position curr
  let colorText = show $ turn pos
  if isAiModeActive curr then do

    liftIOInGame $ putStrLn $ printf "AI is thinking... (this may take a while)"
    moves <- liftIOInGame $ findBestMoveN pos 6 5
    let (move, eval) = head moves
    liftIOInGame $ putStrLn $ "Best Moves: " ++ show moves

    case makeMove pos move of
      Left errMsg -> error $ "AI made invalid move (exiting): " ++ errMsg
      Right newPos -> do
        liftIOInGame $ putStrLn $ "AI played: " ++ show move ++ " with evaluation " ++ show eval
        handleNewPos newPos move

  else do
    liftIOInGame $ putStrLn $ printf "Enter %s's move (e.g., 'pe2e4'):" colorText
    input <- liftIOInGame getLine

    case words $ padInput input of
      [c, arg]
        | c `elem` [":help", ":h"] -> do
          help
          go

        | c `elem` [":undo", ":u"] -> do
          case curr of
            Start _ _ -> do
              liftIOInGame $ putStrLn "Cannot undo from start position."
              go
            Game _ _ prev _ -> do
              put prev
              printPos
              go

        | c `elem` [":fen", ":f"] -> do
              liftIOInGame $ putStrLn $ "Current FEN: " ++ FENParser.posToFEN pos
              go

        | c `elem` [":load", ":l"] -> do
              loadFEN
              go

        | c `elem` [":evaluate", ":e"] -> do
              let depth = readMaybe arg :: Maybe Int
              let defaultDepth = 6 -- Set your default depth here
              let evalDepth = fromMaybe defaultDepth depth
              liftIOInGame $ putStrLn $ "Evaluating position to depth " ++ show evalDepth
              moves <- liftIOInGame $ findBestMoveN pos evalDepth 10
              liftIOInGame $ putStrLn $ "Best Moves: " ++ show moves
              go

        | c `elem` [":ai", ":a"] -> do
              case arg of
                "w" -> do
                  liftIOInGame $ putStrLn "AI will play for white."
                  put $ toggleAiMode curr White
                  go
                "b" -> do
                  liftIOInGame $ putStrLn "AI will play for black."
                  put $ toggleAiMode curr Black
                  go
                _ -> do
                  liftIOInGame $ putStrLn "Invalid color. Please enter a valid color (w or b)."
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

      _ -> do
        liftIOInGame $ putStrLn "Invalid input. Please enter a valid move."
        go

   where
     padInput :: String -> String
     padInput input = if length (words input) < 2 then
        padInput $ input ++ " " ++ "."
      else
        input

help :: GameState ()
help = do
  liftIOInGame $ putStrLn "Commands:"
  liftIOInGame $ putStrLn ":f :fen - get current FEN"
  liftIOInGame $ putStrLn ":l :load - load a FEN"
  liftIOInGame $ putStrLn ":u :undo - undo last move"
  liftIOInGame $ putStrLn ":e :evaluate <depth> - evaluate current position at depth <depth> default 6"
  liftIOInGame $ putStrLn ":a :ai <color> - AI will play for <color> (w or b)"
  liftIOInGame $ putStrLn ":ad <depth> - AI will play at depth <depth> default 6"
  liftIOInGame $ putStrLn ":h :help - print this help message"

printPos :: GameState ()
printPos = do
  curr <- get
  case curr of
    Start pos _ -> do
      liftIOInGame $ putStrLn $ pretty pos
    Game pos _ _ _ -> do
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
          put $ Start pos (GameStateOptions { aiModeWhite = False, aiModeBlack = False })
        Left error -> do
          liftIOInGame $ putStrLn $ "Invalid FEN. Please enter a valid FEN: " ++ error
          loadFEN

handleNewPos :: Position -> Move -> GameState ()
handleNewPos newPos move = do
  liftIOInGame $ putStrLn $ pretty newPos
  curr <- get
  case curr of
    Start currPos opt -> do
      put $ Game newPos move curr opt
    Game _ _ prev opt -> do
      put $ Game newPos move curr opt
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

toggleAiMode :: Game -> Color -> Game
toggleAiMode (Start pos options) color =
  Start pos options'
  where
    options' =
      case color of
        White -> options { aiModeWhite = not $ aiModeWhite options }
        Black -> options { aiModeBlack = not $ aiModeBlack options }
toggleAiMode (Game pos move prev options) color =
  Game pos move prev options'
  where
    options' =
      case color of
        White -> options { aiModeWhite = not $ aiModeWhite options }
        Black -> options { aiModeBlack = not $ aiModeBlack options }
