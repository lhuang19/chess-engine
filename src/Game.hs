module Game
  (gameLoop)
  where

import Control.Monad.State 
import Control.Monad.IO.Class (MonadIO, liftIO)

import MoveParser
import Moves
import Print
import Syntax

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
    CastMove castle -> return () -- todo
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
  let b = board pos
  let b' = updateBoard b from Empty
  let b'' = updateBoard b' to (Occupied (turnColor $ turn pos) piece)
  pos { board = b'', turn = switchColor (turn pos) }

applyCastleMove :: Castle -> Position -> Position
applyCastleMove Kingside pos = undefined

applyCastleMove Queenside pos = undefined

applyPromotionMove :: Promotion -> Position -> Position
applyPromotionMove (Promotion from to piece) pos = undefined

switchColor :: Color -> Color
switchColor White = Black
switchColor Black = White

turnColor :: Color -> Color
turnColor White = White
turnColor Black = Black

gameLoop :: IO ()
gameLoop = go' startingPosition
  where
    go' :: Position -> IO ()
    go' pos = do
      putStrLn $ pretty pos
      go pos

    go :: Position -> IO ()
    go pos = do
      putStrLn $ "Enter your move (e.g., 'pe2e4'):"
      input <- getLine
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
