module Util
  ( annotatedBoard,
    piecesByColor,
    promotionPieces,
    stdMove,
    moveToCoordinate,
    emptyBoard,
    updateAtIndex,
    updateRowAt,
    updateBoard,
    updateBoardSimpleMove,
    initializeBoard,
    rankOp,
    fileOp,
    flipColor,
    predFile,
    succFile,
    predRank,
    succRank,
    fileIndex,
    rankIndex,
    squareAt,
    isSquareEmpty,
    isSquareOccupied,
    canMoveOrCapture,
    stepCoordinate,
    reachableCoordinates,
    CoordinateMove (..),
    FileMove (..),
    RankMove (..),
  )
where

import Control.Monad (foldM, (>=>))
import Data.Maybe (mapMaybe)
import Syntax
import Test.QuickCheck (Arbitrary (..), Gen, Property, elements, oneof, vector)

annotatedBoard :: Board -> [(Coordinate, Square)]
annotatedBoard (Board rows) =
  concat $
    zipWith
      ( \i (Row squares) ->
          zipWith
            ( \j square ->
                (Coordinate j i, square)
            )
            [A .. H]
            squares
      )
      [R1 .. R8]
      rows

piecesByColor :: Color -> Board -> [(Coordinate, Piece)]
piecesByColor color =
  mapMaybe
    ( \(coord, square) -> case square of
        Occupied color' piece
          | color == color' -> Just (coord, piece)
          | otherwise -> Nothing
        Empty -> Nothing
    )
    . annotatedBoard

promotionPieces :: [Piece]
promotionPieces = [Queen, Rook, Knight, Bishop]

stdMove :: Piece -> Coordinate -> Coordinate -> Move
stdMove piece from to = StdMove (StandardMove piece from to)

moveToCoordinate :: Color -> Move -> Coordinate
moveToCoordinate _ (StdMove (StandardMove _ _ to)) = to
moveToCoordinate _ (PromMove (Promotion _ to _)) = to
moveToCoordinate White (CastMove Queenside) = Coordinate C R1
moveToCoordinate White (CastMove Kingside) = Coordinate G R1
moveToCoordinate Black (CastMove Queenside) = Coordinate C R8
moveToCoordinate Black (CastMove Kingside) = Coordinate G R8

emptyBoard :: Board
emptyBoard = Board $ replicate 8 (Row $ replicate 8 Empty)

updateAtIndex :: Int -> a -> [a] -> [a]
updateAtIndex _ _ [] = []
updateAtIndex 0 newVal (x : xs) = newVal : xs
updateAtIndex index newVal (x : xs) =
  x : updateAtIndex (index - 1) newVal xs

updateRowAt :: Int -> (a -> a) -> [a] -> [a]
updateRowAt _ _ [] = []
updateRowAt 0 f (x : xs) = f x : xs
updateRowAt index f (x : xs) =
  x : updateRowAt (index - 1) f xs

updateBoard :: Coordinate -> Square -> Board -> Board
updateBoard (Coordinate f r) sq (Board rows) =
  let updatedRow =
        updateRowAt
          (fromEnum r)
          (\(Row squares) -> Row $ updateAtIndex (fromEnum f) sq squares)
          rows
   in Board updatedRow

updateBoardSimpleMove :: Board -> Coordinate -> Coordinate -> Board
updateBoardSimpleMove b from to =
  let square = squareAt b from
   in updateBoard from Empty $ updateBoard to square b

initializeBoard :: [(Coordinate, Square)] -> Board
initializeBoard = foldl (\b (c, s) -> updateBoard c s b) emptyBoard

rankOp :: Rank -> Rank
rankOp r = case r of
  R1 -> R8
  R2 -> R7
  R3 -> R6
  R4 -> R5
  R5 -> R4
  R6 -> R3
  R7 -> R2
  R8 -> R1

fileOp :: File -> File
fileOp f = case f of
  A -> H
  B -> G
  C -> F
  D -> E
  E -> D
  F -> C
  G -> B
  H -> A

flipColor :: Color -> Color
flipColor White = Black
flipColor Black = White

type FileMove = File -> Maybe File

predFile, succFile :: FileMove
predFile f = case f of
  A -> Nothing
  B -> Just A
  C -> Just B
  D -> Just C
  E -> Just D
  F -> Just E
  G -> Just F
  H -> Just G
succFile f = case f of
  H -> Nothing
  G -> Just H
  F -> Just G
  E -> Just F
  D -> Just E
  C -> Just D
  B -> Just C
  A -> Just B

type RankMove = Rank -> Maybe Rank

predRank, succRank :: RankMove
predRank r = case r of
  R1 -> Nothing
  R2 -> Just R1
  R3 -> Just R2
  R4 -> Just R3
  R5 -> Just R4
  R6 -> Just R5
  R7 -> Just R6
  R8 -> Just R7
succRank r = case r of
  R8 -> Nothing
  R7 -> Just R8
  R6 -> Just R7
  R5 -> Just R6
  R4 -> Just R5
  R3 -> Just R4
  R2 -> Just R3
  R1 -> Just R2

type CoordinateMove = (FileMove, RankMove)

-- Apply the given CoordinateMove to the Coordinate, guarding against
-- boundary overruns and running into our own piece
stepCoordinate :: Position -> Coordinate -> CoordinateMove -> Maybe Coordinate
stepCoordinate p c cm = fst <$> stepCoordinate' p c cm

-- Helper function that reveals whether the step resulted in a capture
stepCoordinate' :: Position -> Coordinate -> CoordinateMove -> Maybe (Coordinate, Bool)
stepCoordinate' pos (Coordinate f r) (fileMove, rankMove) = do
  newFile <- fileMove f
  newRank <- rankMove r
  let newCoord = Coordinate newFile newRank
  let color = turn pos
  let newSquare = squareAt (board pos) newCoord
  case newSquare of
    Empty -> return (newCoord, False)
    Occupied color' _
      | color /= color' -> return (newCoord, True)
      | otherwise -> Nothing

reachableCoordinates :: Position -> Coordinate -> CoordinateMove -> [Coordinate]
reachableCoordinates pos coord move = case stepCoordinate' pos coord move of
  Just (newCoord, capture) ->
    newCoord
      : ( if capture
            then []
            else reachableCoordinates pos newCoord move
        )
  Nothing -> []

fileIndex :: File -> Int
fileIndex f = case f of
  A -> 0
  B -> 1
  C -> 2
  D -> 3
  E -> 4
  F -> 5
  G -> 6
  H -> 7

rankIndex :: Rank -> Int
rankIndex r = case r of
  R1 -> 0
  R2 -> 1
  R3 -> 2
  R4 -> 3
  R5 -> 4
  R6 -> 5
  R7 -> 6
  R8 -> 7

squareAt :: Board -> Coordinate -> Square
squareAt (Board rows) (Coordinate f r) = case rows !! rankIndex r of
  Row squares -> squares !! fileIndex f

isSquareEmpty :: Board -> Coordinate -> Bool
isSquareEmpty b c = squareAt b c == Empty

isSquareOccupied :: Board -> Coordinate -> Bool
isSquareOccupied b c = not $ isSquareEmpty b c

canMoveOrCapture :: Board -> Color -> Coordinate -> Bool
canMoveOrCapture b color coord = case squareAt b coord of
  Occupied color' _ -> color /= color'
  Empty -> True
