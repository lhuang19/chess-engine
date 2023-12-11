module Engine
  ( minimaxAlphaBeta,
    evaluate,
    findBestMove,
    findBestMoveN,
    qc,
  )
where

import Control.Monad (liftM, when)
import Control.Monad.State
import Data.List (maximumBy, minimumBy, sortBy, sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..), comparing)
import Moves
import Syntax
import Test.QuickCheck (Arbitrary (..), Gen, Property, Result, choose, counterexample, discard, elements, forAll, isSuccess, oneof, property, quickCheck, quickCheckResult, (==>))
import Util

type EvalCountState a = StateT (Int, Map.Map Int Int) IO a

incrementEvalCount :: EvalCountState ()
incrementEvalCount = do
  modify (\(x, y) -> (x + 1, y))
  (evalCount, _) <- get
  when (evalCount `mod` 100000 == 0) $ liftIO $ putStrLn $ "Positions evaluated: " ++ show evalCount

incrementPruneCount :: Int -> EvalCountState ()
incrementPruneCount depth = do
  currentMap <- gets snd
  let currentCount = Map.findWithDefault 0 depth currentMap
  let updatedMap = Map.insert depth (currentCount + 1) currentMap
  modify (\(x, y) -> (x, updatedMap))
  when (currentCount + 1 `mod` 100000 == 0) $
    liftIO $
      putStrLn $
        "Positions pruned at depth " ++ show depth ++ ": " ++ show currentCount

runEvalCount :: EvalCountState a -> IO a
runEvalCount action = do
  (result, (evalCount, pruneCount)) <- runStateT action (0, Map.empty)
  putStrLn $ "Positions evaluated: " ++ show evalCount
  putStrLn $ "Positions pruned: " ++ show pruneCount
  let totalPruneCount = sum $ Map.elems pruneCount
  -- Multiply by 2^(depth - 1) to account for the fact that we only count the root node once
  -- There will be some error here because we don't know the depth of the root node
  -- and we may prune subtrees
  let extrapolatedPruneCount = sum $ map (\(depth, count) -> count * 2 ^ (depth - 1)) $ Map.toList pruneCount
  putStrLn $ "Total positions: " ++ show (evalCount + totalPruneCount)
  putStrLn $ "Total positions extrapolated: " ++ show (evalCount + extrapolatedPruneCount)
  return result

data Evaluation
  = BlackMateIn Int [Move]
  | Eval Double
  | WhiteMateIn Int [Move]
  deriving (Show, Eq)

instance Ord Evaluation where
  compare (BlackMateIn x _) (BlackMateIn y _) = compare y x
  compare (WhiteMateIn x _) (WhiteMateIn y _) = compare y x
  compare (Eval x) (Eval y) = compare x y
  compare (BlackMateIn _ _) _ = LT
  compare _ (BlackMateIn _ _) = GT
  compare (WhiteMateIn _ _) _ = GT
  compare _ (WhiteMateIn _ _) = LT

updateEvaluation :: Evaluation -> Move -> Evaluation
updateEvaluation (BlackMateIn x xs) m = BlackMateIn (x + 1) (m : xs)
updateEvaluation (Eval x) _ = Eval x
updateEvaluation (WhiteMateIn x xs) m = WhiteMateIn (x + 1) (m : xs)

minimaxAlphaBeta :: Int -> Evaluation -> Evaluation -> Position -> (Evaluation, Move) -> EvalCountState Evaluation
minimaxAlphaBeta depth alpha beta pos@(Position _ t _ _ _ _) (e', m') =
  case isTerminalEvaluation pos of
    Just e -> return e
    Nothing -> do
      if depth == 0
        then do
          incrementEvalCount
          return $ fastEvaluate e' pos m'
        else -- return $ evaluate pos

          if t == White
            then do
              case children of
                [] -> error "should have terminated"
                ((xMove, xPos) : xs) -> do
                  e <- minimaxAlphaBeta (depth - 1) alpha beta xPos (e', m')
                  let updatedNextEval = updateEvaluation e xMove
                  foldr
                    ( \(childMove, childPos) acc -> do
                        e <- acc
                        if e >= beta || updatedNextEval >= beta
                          then do
                            incrementPruneCount depth
                            return e -- Prune
                          else do
                            nextEval <- minimaxAlphaBeta (depth - 1) (max alpha e) beta childPos (e', childMove)
                            let updatedNextEval = updateEvaluation nextEval childMove
                            return (max updatedNextEval e)
                    )
                    (return updatedNextEval)
                    xs
            else do
              case children of
                [] -> error "should have terminated"
                ((xMove, xPos) : xs) -> do
                  e <- minimaxAlphaBeta (depth - 1) alpha beta xPos (e', m')
                  let updatedNextEval = updateEvaluation e xMove
                  foldr
                    ( \(childMove, childPos) acc -> do
                        e <- acc
                        if e <= alpha
                          then do
                            incrementPruneCount depth
                            return e -- Prune
                          else do
                            nextEval <- minimaxAlphaBeta (depth - 1) alpha (min beta e) childPos (e', childMove)
                            let updatedNextEval = updateEvaluation nextEval childMove
                            return (min updatedNextEval e)
                    )
                    (return updatedNextEval)
                    xs
  where
    children = validMoves pos

    isTerminalEvaluation :: Position -> Maybe Evaluation
    isTerminalEvaluation pos@(Position _ t _ _ halfMove fullMove)
      | halfMove >= 100 || fullMove >= 500 = Just $ Eval 0
      | otherwise = case gameCondition pos of
          Checkmate -> case t of
            White -> Just $ BlackMateIn 0 []
            Black -> Just $ WhiteMateIn 0 []
          Stalemate -> Just $ Eval 0
          _ -> Nothing

pieceValue :: Piece -> Double
pieceValue Pawn = 1.0
pieceValue Knight = 3.0
pieceValue Bishop = 3.25
pieceValue Rook = 5.0
pieceValue Queen = 9.0
pieceValue King = 1000.0 -- Assign a high value to the king for checkmate detection

evaluate :: Position -> Evaluation
evaluate pos@(Position (Board rows) _ _ _ _ _) = do
  case gameCondition pos of
    Checkmate -> case turn pos of
      White -> BlackMateIn 0 []
      Black -> WhiteMateIn 0 []
    Stalemate -> Eval 0
    _ -> Eval $ sum $ map countPieceValue (concatMap (\(Row squares) -> squares) rows)
  where
    countPieceValue :: Square -> Double
    countPieceValue (Occupied color piece) =
      case color of
        White -> pieceValue piece
        Black -> -pieceValue piece
    countPieceValue Empty = 0

fastEvaluate :: Evaluation -> Position -> Move -> Evaluation
fastEvaluate (BlackMateIn x xs) _ m = BlackMateIn (x + 1) (m : xs)
fastEvaluate (WhiteMateIn x xs) _ m = WhiteMateIn (x + 1) (m : xs)
fastEvaluate (Eval x) pos m =
  case m of
    StdMove (StandardMove {}) -> Eval x
    CastMove _ -> Eval x
    PromMove (Promotion _ to piece) ->
      case squareAt (board pos) to of
        Occupied _ piece' ->
          if turn pos == White
            then Eval $ pieceValue piece + pieceValue piece' - pieceValue Pawn
            else Eval $ -pieceValue piece - pieceValue piece' + pieceValue Pawn
        Empty ->
          if turn pos == White
            then Eval $ pieceValue piece - pieceValue Pawn
            else Eval $ -pieceValue piece + pieceValue Pawn

evalFunc :: Int -> (Move, Position) -> EvalCountState Evaluation
evalFunc depth (m, p) = minimaxAlphaBeta (depth - 1) (Eval $ -1 / 0) (Eval $ 1 / 0) p (evaluate p, m)

findBestMove' :: Position -> Int -> EvalCountState (Move, Evaluation)
findBestMove' pos depth = do
  evaledMoves <- mapZipM (evalFunc depth) (validMoves pos)
  let compareFunc (_, e1) (_, e2) = compare e1 e2
  case turn pos of
    White -> do
      let ((m, _), e) = maximumBy compareFunc evaledMoves
      return (m, updateEvaluation e m)
    Black -> do
      let ((m, _), e) = minimumBy compareFunc evaledMoves
      return (m, updateEvaluation e m)

findBestMoveN' :: Position -> Int -> Int -> EvalCountState [(Move, Evaluation)]
findBestMoveN' pos depth n = do
  evaledMoves <- mapZipM (evalFunc depth) (validMoves pos)
  let compareFunc (_, e1) (_, e2) = compare e1 e2
  let valueFunc (_, e) = e
  case turn pos of
    White -> do
      let sorted = sortOn (Down . valueFunc) evaledMoves
      return $ take n $ map (\((m, _), e) -> (m, updateEvaluation e m)) sorted
    Black -> do
      let sorted = sortOn valueFunc evaledMoves
      return $ take n $ map (\((m, _), e) -> (m, updateEvaluation e m)) sorted

-- Run the state monad to track the number of positions evaluated
findBestMove :: Position -> Int -> IO (Move, Evaluation)
findBestMove pos depth = runEvalCount $ findBestMove' pos depth

-- Run the state monad to track the number of positions evaluated
findBestMoveN :: Position -> Int -> Int -> IO [(Move, Evaluation)]
findBestMoveN pos depth n = runEvalCount $ findBestMoveN' pos depth n

comparingM :: (Ord b, Monad m) => (a -> m b) -> a -> a -> m Ordering
comparingM f x y = do
  x' <- f x
  y' <- f y
  return $ compare x' y'

maximumByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m a
maximumByM cmp xs =
  foldM (\acc x -> do c <- cmp acc x; return $ if c == GT then acc else x) (head xs) (tail xs)

minimumByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m a
minimumByM cmp xs =
  foldM (\acc x -> do c <- cmp acc x; return $ if c == LT then acc else x) (head xs) (tail xs)

mapZipM :: (Monad m) => (a -> m b) -> [a] -> m [(a, b)]
mapZipM f = mapM (\x -> f x >>= \y -> return (x, y))

sortOnM :: (Ord b, Monad m) => (a -> m b) -> [a] -> m [a]
sortOnM f list = do
  zipped <- mapZipM f list
  return $ map fst $ sortOn snd zipped

prop_depthEval :: Position -> Bool
prop_depthEval pos = undefined

qc :: IO [Result]
qc =
  sequence
    [ putStrLn "prop_depthEval" >> quickCheckResult prop_depthEval
    ]
