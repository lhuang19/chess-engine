module Syntax
  ( wBoard1,
    wPosition1,
    Board (..),
    Row (..),
    Square (..),
    Piece (..),
    Color (..),
    File (..),
    Rank (..),
    Castling (..),
    Coordinate (..),
    Position (..),
    StandardMove (..),
    Castle (..),
    Promotion (..),
    Move (..),
    Game (..),
    annotatedBoard,
    piecesByColor,
    predFile,
    succFile,
    predRank,
    succRank,
    squareAt,
    Dir' (..),
    Bounds (..),
    Dir (..),
    dirOpposite,
    rankOp,
    fileOp,
    colorOp,
    coordinateMove,
    coordinateMoveMultiDir,
    coordinateMoveValid,
    coordinateMoveMultiDirValid,
    coordinateMoves,
    coordinateMovesMultiDir,
    coordinateValidMoves,
    coordinateMovesMultiDirValid,
    emptyBoard,
    updateBoard,
    initializeBoard,
    startingPosition,
  )
where

import Control.Monad (foldM, (>=>))
import Data.Maybe (mapMaybe)

newtype Row = Row [Square] deriving (Show, Eq)

newtype Board = Board [Row] deriving (Show, Eq)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

data Color = White | Black deriving (Show, Eq)

data Square = Empty | Occupied Color Piece deriving (Show, Eq)

data File = A | B | C | D | E | F | G | H deriving (Show, Eq, Enum, Bounded)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Show, Eq, Enum, Bounded)

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
piecesByColor c =
  map (\(c', s) -> case s of Occupied _ p -> (c', p); _ -> error "Empty square")
    . filter (\(_, s) -> case s of Occupied c' _ -> c == c'; _ -> False)
    . annotatedBoard

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
updateBoard (Coordinate r f) sq (Board rows) =
  let updatedRow =
        updateRowAt
          (fromEnum f)
          (\(Row squares) -> Row $ updateAtIndex (fromEnum r) sq squares)
          rows
   in Board updatedRow

initializeBoard :: [(Coordinate, Square)] -> Board
initializeBoard = foldl (\b (c, s) -> updateBoard c s b) emptyBoard

data Castling = Castling
  { whiteKingSide :: Bool,
    whiteQueenSide :: Bool,
    blackKingSide :: Bool,
    blackQueenSide :: Bool
  }
  deriving (Show, Eq)

data Coordinate = Coordinate
  { file :: File,
    rank :: Rank
  }
  deriving (Show, Eq)

-- TODO move this to separate file

data Dir'
  = SF -- succ file
  | PF -- pred file
  | SR -- succ rank
  | PR -- pred rank
  deriving (Show, Eq)

data Bounds = CanCapture | MustCapture | CannotCapture deriving (Show, Eq)

data Dir = Dir Bounds [Dir'] deriving (Show, Eq)

dirOp :: Dir' -> Coordinate -> Maybe Coordinate
dirOp d (Coordinate f r) = case d of
  SF -> Coordinate <$> succFile f <*> pure r
  PF -> Coordinate <$> predFile f <*> pure r
  SR -> Coordinate f <$> succRank r
  PR -> Coordinate f <$> predRank r

dirOpposite' :: Dir' -> Dir'
dirOpposite' d = case d of
  SF -> PF
  PF -> SF
  SR -> PR
  PR -> SR

dirOpposite :: Dir -> Dir
dirOpposite (Dir b ds) = Dir b $ map dirOpposite' ds

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

colorOp :: Color -> Color
colorOp White = Black
colorOp Black = White

coordinateMove :: Dir -> Coordinate -> Maybe Coordinate
coordinateMove (Dir _ ds) c = foldM (flip dirOp) c ds

coordinateMoveValid :: Dir -> Position -> Coordinate -> Maybe Coordinate
coordinateMoveValid d@(Dir bounds _) (Position b t _ _ _ _) c = do
  c' <- coordinateMove d c
  case (bounds, squareAt b c') of
    (CanCapture, Occupied t' _) -> if t == t' then Nothing else Just c'
    (MustCapture, Occupied t' _) -> if t == t' then Nothing else Just c'
    (CannotCapture, Occupied t' _) -> Nothing
    (CanCapture, Empty) -> Just c'
    (MustCapture, Empty) -> Nothing
    (CannotCapture, Empty) -> Just c'

coordinateMoveMultiDir :: [Dir] -> Coordinate -> [Coordinate]
coordinateMoveMultiDir ds c = mapMaybe (`coordinateMove` c) ds

coordinateMoveMultiDirValid :: [Dir] -> Position -> Coordinate -> [Coordinate]
coordinateMoveMultiDirValid ds p c = mapMaybe (\d -> coordinateMoveValid d p c) ds

iterateM' :: (a -> Maybe a) -> a -> [a]
iterateM' f x = x : maybe [] (iterateM' f) (f x)

iterateM :: (a -> Maybe a) -> a -> [a]
iterateM f x = tail $ iterateM' f x

coordinateMoves :: Dir -> Coordinate -> [Coordinate]
coordinateMoves d = iterateM (coordinateMove d)

coordinateValidMoves :: Dir -> Position -> Coordinate -> [Coordinate]
coordinateValidMoves d@(Dir bounds _) p = iterateM (coordinateMoveValid d p)

coordinateMovesMultiDir :: [Dir] -> Coordinate -> [Coordinate]
coordinateMovesMultiDir ds c = concatMap (`coordinateMoves` c) ds

coordinateMovesMultiDirValid :: [Dir] -> Position -> Coordinate -> [Coordinate]
coordinateMovesMultiDirValid ds p c = concatMap (\d -> coordinateValidMoves d p c) ds

data Position = Position
  { board :: Board,
    turn :: Color,
    castling :: Castling,
    enPassant :: Maybe Coordinate,
    halfMoveClock :: Int,
    fullMoveNumber :: Int
  }
  deriving (Show, Eq)

-- Assume user puts in relatively well formed data

data StandardMove = StandardMove Piece Coordinate Coordinate deriving (Show, Eq)

data Castle = Kingside | Queenside deriving (Show, Eq)

data Promotion = Promotion Coordinate Coordinate Piece deriving (Show, Eq)

data Move
  = StdMove StandardMove
  | CastMove Castle
  | PromMove Promotion
  deriving (Show, Eq)

data Game = Game
  { position :: Position,
    previousPosition :: Position,
    lastMove :: Maybe Move
  }
  deriving (Show, Eq)

-- functions on data types
predFile, succFile :: File -> Maybe File
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

predRank, succRank :: Rank -> Maybe Rank
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

-- test1
wBoard1 :: Board
wBoard1 =
  Board
    [ Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Occupied White King],
      Row [Occupied White Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty],
      Row [Empty, Empty, Occupied White Pawn, Empty, Occupied White Pawn, Empty, Occupied White Pawn, Occupied Black King],
      Row [Empty, Empty, Empty, Empty, Empty, Empty, Occupied Black Pawn, Occupied White Pawn],
      Row [Empty, Empty, Empty, Occupied Black Pawn, Empty, Empty, Occupied White Pawn, Empty],
      Row [Empty, Occupied White Pawn, Empty, Empty, Empty, Empty, Empty, Empty],
      Row [Empty, Empty, Empty, Occupied White Pawn, Empty, Empty, Empty, Empty],
      Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
    ]

wPosition1 :: Position
wPosition1 =
  Position
    wBoard1
    White
    (Castling False False False False)
    Nothing
    0
    1

startingPosition :: Position
startingPosition =
  Position
    ( Board
        [ Row [Occupied White Rook, Occupied White Knight, Occupied White Bishop, Occupied White Queen, Occupied White King, Occupied White Bishop, Occupied White Knight, Occupied White Rook],
          Row [Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn],
          Row [Occupied Black Rook, Occupied Black Knight, Occupied Black Bishop, Occupied Black Queen, Occupied Black King, Occupied Black Bishop, Occupied Black Knight, Occupied Black Rook]
        ]
    )
    White
    (Castling True True True True)
    Nothing
    0
    1
