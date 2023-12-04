module Game
  (gameLoop)
  where

import Control.Monad.State 
import Control.Monad.IO.Class (MonadIO, liftIO)

import MoveParser
import Moves
import Print
import Syntax
import FENParser

type GameMonad a = State Game a

makeMove :: Move -> GameMonad (Either String ())
makeMove m = do
  currentPos <- gets position
  case moveValid m currentPos of
    Left err -> return $ Left err
    Right () -> do
      let newPos = applyMove m currentPos
      modify (\s -> s { position = newPos, previousPosition = currentPos })
      return $ Right ()

getCurrentPosition :: GameMonad Position
getCurrentPosition = gets position

moveValid :: Move -> Position -> Either String ()
moveValid m pos = 
  case m of
    CastMove castle ->
      if validCastle pos castle
      then return ()
      else Left "Invalid castle"
    PromMove prom -> return () -- todo
    StdMove (StandardMove p from to) ->
      if to `elem` validMoves pos p from
      then return ()
      else Left "Invalid move"

applyMove :: Move -> Position -> Position
applyMove move pos = case move of
  StdMove stdMove -> applyStandardMove stdMove pos
  CastMove castle -> applyCastleMove castle pos
  PromMove prom -> applyPromotionMove prom pos

applyStandardMove :: StandardMove -> Position -> Position
applyStandardMove (StandardMove piece from to) pos = do
  \b -> pos { board = b, turn = colorOp (turn pos) }
  $ updateBoard from Empty
  $ updateBoard to (Occupied (turnColor $ turn pos) piece)
  $ board pos

applyCastleMove :: Castle -> Position -> Position
applyCastleMove Kingside pos = do
  \b -> pos { board = b, turn = colorOp (turn pos) }
  $ updateBoard (Coordinate E r) Empty
  $ updateBoard (Coordinate H r) Empty
  $ updateBoard (Coordinate F r) (Occupied (turnColor $ turn pos) King)
  $ updateBoard (Coordinate G r) (Occupied (turnColor $ turn pos) Rook)
  $ board pos
  where
    r = if turn pos == White then R1 else R8

applyCastleMove Queenside pos = do
  \b -> pos { board = b, turn = colorOp (turn pos) }
  $ updateBoard (Coordinate E r) Empty
  $ updateBoard (Coordinate A r) Empty
  $ updateBoard (Coordinate C r) (Occupied (turnColor $ turn pos) King)
  $ updateBoard (Coordinate D r) (Occupied (turnColor $ turn pos) Rook)
  $ board pos
  where
    r = if turn pos == White then R1 else R8

applyPromotionMove :: Promotion -> Position -> Position
applyPromotionMove (Promotion from to piece) pos = do
  \b -> pos { board = b, turn = colorOp (turn pos) }
  $ updateBoard from Empty
  $ updateBoard to (Occupied (turnColor $ turn pos) piece)
  $ board pos


turnColor :: Color -> Color
turnColor White = White
turnColor Black = Black

gameLoop :: IO ()
gameLoop = go' startingPosition
  where
    go' :: Position -> IO ()
    go' pos = do
      putStrLn $ "Type FEN at any time to get current FEN"
      putStrLn $ pretty pos
      go pos

    go :: Position -> IO ()
    go pos = do
      putStrLn $ "Enter your move (e.g., 'pe2e4'):"
      input <- getLine
      if input == "FEN"
      then do
        putStrLn $ "Current FEN: " ++ FENParser.posToFEN pos
        go pos
      else
        case parseMove input of
          Right move -> do
            case moveValid move pos of
              Left errMsg -> do
                putStrLn $ "Invalid move: " ++ errMsg
                go pos
              Right () -> do
                let newPos = applyMove move pos
                putStrLn $ "New Position: " ++ pretty newPos
                putStrLn $ pretty newPos
                go newPos
          Left error -> do
            putStrLn $ "Invalid input. Please enter a valid move." ++ error
            go pos
