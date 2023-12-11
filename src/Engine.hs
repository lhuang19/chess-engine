module Engine
  ( minimaxAlphaBeta,
    evaluate,
    findBestMove,
    findBestMoveN,
    qc,
  )
where

import System.Random
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

minimaxAlphaBeta :: Int -> Evaluation -> Evaluation -> Position -> EvalCountState Evaluation
minimaxAlphaBeta depth alpha beta pos@(Position _ t _ _ _ _) =
  case isTerminalEvaluation pos of
    Just e -> return e
    Nothing -> do
      if depth == 0
        then do
          incrementEvalCount
          return $ evaluate pos
        else

          if t == White
            then do
              case children of
                [] -> error "should have terminated"
                ((xMove, xPos) : xs) -> do
                  e <- minimaxAlphaBeta (depth - 1) alpha beta xPos
                  let updatedNextEval = updateEvaluation e xMove
                  foldr
                    ( \(childMove, childPos) acc -> do
                        e <- acc
                        if e >= beta || updatedNextEval >= beta
                          then do
                            incrementPruneCount depth
                            return e -- Prune
                          else do
                            nextEval <- minimaxAlphaBeta (depth - 1) (max alpha e) beta childPos 
                            let updatedNextEval = updateEvaluation nextEval childMove
                            return (max updatedNextEval e)
                    )
                    (return updatedNextEval)
                    xs
            else do
              case children of
                [] -> error "should have terminated"
                ((xMove, xPos) : xs) -> do
                  e <- minimaxAlphaBeta (depth - 1) alpha beta xPos
                  let updatedNextEval = updateEvaluation e xMove
                  foldr
                    ( \(childMove, childPos) acc -> do
                        e <- acc
                        if e <= alpha
                          then do
                            incrementPruneCount depth
                            return e -- Prune
                          else do
                            nextEval <- minimaxAlphaBeta (depth - 1) alpha (min beta e) childPos 
                            let updatedNextEval = updateEvaluation nextEval childMove
                            return (min updatedNextEval e)
                    )
                    (return updatedNextEval)
                    xs
  where
    children :: [(Move, Position)]
    children =
      let gen = mkStdGen 42 -- You can replace 42 with any seed value
          (shuffledMoves, _) = shuffle gen $ validMoves pos
      in shuffledMoves

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
pieceValue Pawn = 1
pieceValue Knight = 3.2
pieceValue Bishop = 3.3
pieceValue Rook = 5.0
pieceValue Queen = 9
pieceValue King = 200

evaluate :: Position -> Evaluation
evaluate pos@(Position board _ _ _ _ _) = do
  case gameCondition pos of
    Checkmate -> case turn pos of
      White -> BlackMateIn 0 []
      Black -> WhiteMateIn 0 []
    Stalemate -> Eval 0
    FiftyMoveDraw -> Eval 0
    _ -> Eval $ sum $ map (countPieceValueWithBonus board) (annotatedBoard board)
  where
    countPieceValueWithBonus :: Board -> (Coordinate, Square) -> Double
    countPieceValueWithBonus _ (coord, Occupied color piece) =
      let baseValue = case color of
            White -> pieceValue piece
            Black -> -pieceValue piece
       in let bonus = case piece of
                Pawn -> pawnPositionBonus coord
                Knight -> knightPositionBonus coord
                Bishop -> bishopPositionBonus coord
                Rook -> rookPositionBonus coord
                Queen -> queenPositionBonus coord
                King -> kingPositionBonus coord
           in baseValue + bonus
    countPieceValueWithBonus _ (_, Empty) = 0

    pawnPositionBonus :: Coordinate -> Double
    pawnPositionBonus (Coordinate file rank) =
      let table =
            [ [0, 0, 0, 0, 0, 0, 0, 0],
              [50, 50, 50, 50, 50, 50, 50, 50],
              [10, 10, 20, 30, 30, 20, 10, 10],
              [5, 5, 10, 25, 25, 10, 5, 5],
              [0, 0, 0, 20, 20, 0, 0, 0],
              [5, -5, -10, 0, 0, -10, -5, 5],
              [5, 10, 10, -20, -20, 10, 10, 5],
              [0, 0, 0, 0, 0, 0, 0, 0]
            ]
       in table !! rankIndex rank !! fileIndex file / 100.0

    knightPositionBonus :: Coordinate -> Double
    knightPositionBonus (Coordinate file rank) =
      let table =
            [ [-50, -40, -30, -30, -30, -30, -40, -50],
              [-40, -20, 0, 0, 0, 0, -20, -40],
              [-30, 0, 10, 15, 15, 10, 0, -30],
              [-30, 5, 15, 20, 20, 15, 5, -30],
              [-30, 0, 15, 20, 20, 15, 0, -30],
              [-30, 5, 10, 15, 15, 10, 5, -30],
              [-40, -20, 0, 5, 5, 0, -20, -40],
              [-50, -40, -30, -30, -30, -30, -40, -50]
            ]
       in table !! rankIndex rank !! fileIndex file / 100.0

    bishopPositionBonus :: Coordinate -> Double
    bishopPositionBonus (Coordinate file rank) =
      let table =
            [ [-20, -10, -10, -10, -10, -10, -10, -20],
              [-10, 0, 0, 0, 0, 0, 0, -10],
              [-10, 0, 5, 10, 10, 5, 0, -10],
              [-10, 5, 5, 10, 10, 5, 5, -10],
              [-10, 0, 10, 10, 10, 10, 0, -10],
              [-10, 10, 10, 10, 10, 10, 10, -10],
              [-10, 5, 0, 0, 0, 0, 5, -10],
              [-20, -10, -10, -10, -10, -10, -10, -20]
            ]
       in table !! rankIndex rank !! fileIndex file / 100.0

    rookPositionBonus :: Coordinate -> Double
    rookPositionBonus (Coordinate file rank) =
      let table =
            [ [0, 0, 0, 0, 0, 0, 0, 0],
              [5, 10, 10, 10, 10, 10, 10, 5],
              [-5, 0, 0, 0, 0, 0, 0, -5],
              [-5, 0, 0, 0, 0, 0, 0, -5],
              [-5, 0, 0, 0, 0, 0, 0, -5],
              [-5, 0, 0, 0, 0, 0, 0, -5],
              [-5, 0, 0, 0, 0, 0, 0, -5],
              [0, 0, 0, 5, 5, 0, 0, 0]
            ]
       in table !! rankIndex rank !! fileIndex file / 100.0

    queenPositionBonus :: Coordinate -> Double
    queenPositionBonus (Coordinate file rank) =
      let table =
            [ [-20, -10, -10, -5, -5, -10, -10, -20],
              [-10, 0, 0, 0, 0, 0, 0, -10],
              [-10, 0, 5, 5, 5, 5, 0, -10],
              [-5, 0, 5, 5, 5, 5, 0, -5],
              [0, 0, 5, 5, 5, 5, 0, -5],
              [-10, 5, 5, 5, 5, 5, 0, -10],
              [-10, 0, 5, 0, 0, 0, 0, -10],
              [-20, -10, -10, -5, -5, -10, -10, -20]
            ]
       in table !! rankIndex rank !! fileIndex file / 100.0

    kingPositionBonus :: Coordinate -> Double
    kingPositionBonus (Coordinate file rank) =
      let table =
            [ [-30, -40, -40, -50, -50, -40, -40, -30],
              [-30, -40, -40, -50, -50, -40, -40, -30],
              [-30, -40, -40, -50, -50, -40, -40, -30],
              [-30, -40, -40, -50, -50, -40, -40, -30],
              [-20, -30, -30, -40, -40, -30, -30, -20],
              [-10, -20, -20, -20, -20, -20, -20, -10],
              [20, 20, 0, 0, 0, 0, 20, 20],
              [20, 30, 10, 0, 0, 10, 30, 20]
            ]
       in table !! rankIndex rank !! fileIndex file / 100.0

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
evalFunc depth (m, p) = minimaxAlphaBeta (depth - 1) (Eval $ -1 / 0) (Eval $ 1 / 0) p

findBestMove' :: Position -> Int -> EvalCountState (Move, Evaluation)
findBestMove' pos depth = do
  let shuffledMoves = fst $ shuffle (mkStdGen 42) $ validMoves pos
  evaledMoves <- mapZipM (evalFunc depth) shuffledMoves
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

prop_depthEval :: Position -> Bool
prop_depthEval pos = undefined

qc :: IO [Result]
qc =
  sequence
    [ putStrLn "prop_depthEval" >> quickCheckResult prop_depthEval
    ]
