module Engine
  ( minimaxAlphaBeta,
    evaluate,
    findBestMove, 
    findBestMoveN
  )

where

import System.Random
import Data.List (maximumBy, minimumBy, sortOn, sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Map.Strict as Map
import Control.Monad (liftM, when)
import Control.Monad.State
import Syntax
import Moves
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
  when ((currentCount + 1) `mod` 100000 == 0)
    $ liftIO
    $ putStrLn
    $ "Positions pruned at depth " ++ show depth ++ ": " ++ show currentCount

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
  | Eval Double (Maybe Move)
  | WhiteMateIn Int [Move]
  deriving (Show, Eq)

eval :: Evaluation -> Double
eval (BlackMateIn x _) = -1000.0 + fromIntegral x
eval (Eval x _) = x
eval (WhiteMateIn x _) = 1000.0 - fromIntegral x

evaluationToMove :: Evaluation -> Maybe Move
evaluationToMove (BlackMateIn _ (x: _)) = Just x
evaluationToMove (Eval _ m) = m
evaluationToMove (WhiteMateIn _ (x: _)) = Just x
evaluationToMove _ = Nothing

evaluationToMoveExn :: Evaluation -> Move
evaluationToMoveExn (BlackMateIn _ (x: _)) = x
evaluationToMoveExn (Eval _ (Just m)) = m
evaluationToMoveExn (WhiteMateIn _ (x: _)) = x
evaluationToMoveExn e = error $ "No move on exn" ++ show e

instance Ord Evaluation where
  compare (BlackMateIn x _) (BlackMateIn y _) = compare y x
  compare (WhiteMateIn x _) (WhiteMateIn y _) = compare y x
  -- Can try to see if we prefer some piece moves over others
  compare (Eval x _) (Eval y _) = compare x y
  compare (BlackMateIn _ _) _ = LT
  compare _ (BlackMateIn _ _) = GT
  compare (WhiteMateIn _ _) _ = GT
  compare _ (WhiteMateIn _ _) = LT

  max w1@(WhiteMateIn x1 _) w2@(WhiteMateIn x2 _) = if x1 > x2 then w2 else w1
  max w@(WhiteMateIn _ _) _ = w
  max _ w@(WhiteMateIn _ _) = w
  max e1@(Eval x _) e2@(Eval y _) = if x > y then e1 else e2
  max e@(Eval x _) _ = e
  max _ e@(Eval y _) = e
  max b1@(BlackMateIn x1 _) b2@(BlackMateIn x2 _) = if x1 > x2 then b1 else b2

  min b1@(BlackMateIn x1 _) b2@(BlackMateIn x2 _) = if x1 > x2 then b2 else b1
  min b@(BlackMateIn _ _) _ = b
  min _ b@(BlackMateIn _ _) = b
  min e1@(Eval x _) e2@(Eval y _) = if x > y then e2 else e1
  min e@(Eval x _) _ = e
  min _ (Eval y e) = Eval y e
  min w1@(WhiteMateIn x1 _) w2@(WhiteMateIn x2 _) = if x1 > x2 then w1 else w2

updateEvaluation :: Evaluation -> Move -> Evaluation
updateEvaluation (BlackMateIn x xs) m = BlackMateIn (x + 1) (m: xs)
updateEvaluation e@(Eval x _) m = Eval x (Just m)
updateEvaluation (WhiteMateIn x xs) m = WhiteMateIn (x + 1) (m: xs)

-- runs minimax with alpha-beta pruning and returns a list of top 5 evaluations
minimaxAlphaBeta :: Int -> Double -> Double -> Position -> (Evaluation, Maybe Move) -> EvalCountState [Evaluation]
minimaxAlphaBeta depth alpha beta pos@(Position _ t _ _ _ _) (e', m') =
  case isTerminalEvaluation pos of
    Just e -> return [e]
    Nothing -> do
      if depth == 0
        then do
            incrementEvalCount
            case m' of
              Just m -> return [fastEvaluate e' pos m]
              Nothing -> return [evaluate pos]
        else if t == White
        then do
          case children of
            [] -> error "should have terminated"
            ((xMove, xPos) : xs) -> do
              es <- minimaxAlphaBeta (depth - 1) alpha beta xPos (e', m')
              let e = head es
              let updatedNextEval = updateEvaluation e xMove
              foldr (\(childMove, childPos) acc -> do
                es <- acc
                let e = head es
                if eval e >= beta || eval updatedNextEval >= beta
                  then do
                    incrementPruneCount depth
                    return es -- Prune
                  else do
                    nextEvals <- minimaxAlphaBeta (depth - 1) (max alpha (eval e)) beta childPos (e', Just childMove)
                    let nextEval = head nextEvals
                    let updatedNextEval = updateEvaluation nextEval childMove
                    -- Keep a sorted list of top 5 evaluations
                    return $ take 5 $ sortBy (flip compare) (updatedNextEval : es)
                ) (return [updatedNextEval]) xs
        else do
          case children of
            [] -> error "should have terminated"
            ((xMove, xPos) : xs) -> do
              es <- minimaxAlphaBeta (depth - 1) alpha beta xPos (e', m')
              let e = head es
              let updatedNextEval = updateEvaluation e xMove
              foldr (\(childMove, childPos) acc -> do
                es <- acc
                let e = head es
                if eval e <= alpha
                  then do
                    incrementPruneCount depth
                    return es -- Prune
                  else do
                    nextEvals <- minimaxAlphaBeta (depth - 1) alpha (min beta (eval e)) childPos (e', Just childMove)
                    let nextEval = head nextEvals
                    let updatedNextEval = updateEvaluation nextEval childMove
                    -- Keep a sorted list of top 5 evaluations
                    return $ take 5 $ sortBy compare (updatedNextEval : es)
                ) (return [updatedNextEval]) xs
  where
    children :: [(Move, Position)]
    children =
      let gen = mkStdGen 42 -- You can replace 42 with any seed value
          (shuffledMoves, _) = shuffle gen $ validMoves pos
      in shuffledMoves
    -- children :: [(Move, Position)]
    -- children = do
    --   gen <- getStdGen
    --   let (shuffledMoves, _) = shuffle gen $ validMoves pos
    --   return shuffledMoves

    isTerminalEvaluation :: Position -> Maybe Evaluation
    isTerminalEvaluation pos@(Position _ t _ _ halfMove fullMove)
      | halfMove >= 100 || fullMove >= 500 = Just $ Eval 0 Nothing
      | otherwise = case gameCondition pos of
        Checkmate -> case t of
          White -> Just $ BlackMateIn 0 []
          Black -> Just $ WhiteMateIn 0 []
        Stalemate -> Just $ Eval 0 Nothing
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
    Stalemate -> Eval 0 Nothing
    _ -> Eval (sum $ map (\(Row xs) -> sum $ map countPieceValue xs) rows) Nothing
  where
    countPieceValue :: Square -> Double
    countPieceValue (Occupied color piece) =
      case color of
        White -> pieceValue piece
        Black -> -pieceValue piece
    countPieceValue Empty = 0

fastEvaluate :: Evaluation -> Position -> Move -> Evaluation
fastEvaluate (BlackMateIn x xs) _ m = BlackMateIn (x + 1) (m: xs)
fastEvaluate (WhiteMateIn x xs) _ m = WhiteMateIn (x + 1) (m: xs)
fastEvaluate (Eval x _) pos m =
  case m of
    StdMove (StandardMove _ _ _) -> Eval x (Just m)
    CastMove _ -> Eval x (Just m)
    PromMove (Promotion _ to piece) ->
      case squareAt (board pos) to of
            Occupied _ piece' ->
              if turn pos == White
                then Eval (pieceValue piece + pieceValue piece' - pieceValue Pawn) (Just m)
                else Eval (-pieceValue piece - pieceValue piece' + pieceValue Pawn) (Just m)
            Empty ->
              if turn pos == White
                then Eval (pieceValue piece - pieceValue Pawn) (Just m)
                else Eval (-pieceValue piece + pieceValue Pawn) (Just m)

-- use new minimax alphabeta that already returns a sorted list
findBestMove' :: Position -> Int -> EvalCountState (Move, Evaluation)
findBestMove' pos depth = do
  es <- minimaxAlphaBeta depth (-1 / 0) (1 / 0) pos (evaluate pos, Nothing)
  let e = head es
  return (evaluationToMoveExn e, e)

findBestMoveN' :: Position -> Int -> Int -> EvalCountState [(Move, Evaluation)]
findBestMoveN' pos depth n = do
  es <- minimaxAlphaBeta depth (-1 / 0) (1 / 0) pos (evaluate pos, Nothing)
  return $ map (\e -> (evaluationToMoveExn e, e)) $ take n es

-- evalFunc :: Int -> (Move, Position) -> EvalCountState Evaluation
-- evalFunc depth (m, p) = minimaxAlphaBeta (depth - 1) (-1 / 0) (1 / 0) p (evaluate p, m)

-- findBestMove' :: Position -> Int -> EvalCountState (Move, Evaluation)
-- findBestMove' pos depth = do
--   evaledMoves <- mapZipM (evalFunc depth) (validMoves pos)
--   let compareFunc (_, e1) (_, e2) = compare e1 e2
--   case turn pos of
--     White -> do
--       let ((m, _), e) = maximumBy compareFunc evaledMoves
--       return (m, updateEvaluation e m)
--     Black -> do
--       let ((m, _), e) = minimumBy compareFunc evaledMoves
--       return (m, updateEvaluation e m)

-- findBestMoveN' :: Position -> Int -> Int -> EvalCountState [(Move, Evaluation)]
-- findBestMoveN' pos depth n = do
--   evaledMoves <- mapZipM (evalFunc depth) (validMoves pos)
--   let compareFunc (_, e1) (_, e2) = compare e1 e2
--   let valueFunc (_, e) = e
--   case turn pos of
--     White -> do
--       let sorted = sortOn (Down . valueFunc) evaledMoves
--       return $ take n $ map (\((m, _), e) -> (m, updateEvaluation e m)) sorted
--     Black -> do
--       let sorted = sortOn valueFunc evaledMoves
--       return $ take n $ map (\((m, _), e) -> (m, updateEvaluation e m)) sorted

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

maximumByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m a
maximumByM cmp xs =
  foldM (\acc x -> do {c <- cmp acc x; return $ if c == GT then acc else x}) (head xs) (tail xs)

minimumByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m a
minimumByM cmp xs =
  foldM (\acc x -> do {c <- cmp acc x; return $ if c == LT then acc else x}) (head xs) (tail xs)

mapZipM :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
mapZipM f = mapM (\x -> f x >>= \y -> return (x, y))

sortOnM :: (Ord b, Monad m) => (a -> m b) -> [a] -> m [a]
sortOnM f list = do
  zipped <- mapZipM f list
  return $ map fst $ sortOn snd zipped

shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen xs = go gen (length xs - 1) xs
  where
    go g 0 lst = (lst, g)  -- Return the entire list
    go g i lst =
      let (j, g') = randomR (0, i) g
          ith = lst !! i
          jth = lst !! j
          lst' = replace i jth $ replace j ith lst
      in go g' (i - 1) lst'

    replace n x lst = take n lst ++ [x] ++ drop (n + 1) lst
