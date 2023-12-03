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
    Capture (..),
    Castle (..),
    EnPassant (..),
    Promotion (..),
    Move (..),
    Game (..),
    annotatedBoard,
    predFile,
    succFile,
    predRank,
    succRank,
    )

where

newtype Row = Row [Square] deriving (Show, Eq)
newtype Board = Board [Row] deriving (Show, Eq)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Color = White | Black deriving (Show, Eq)
data Square = Empty | Occupied Color Piece deriving (Show, Eq)

data File = A | B | C | D | E | F | G | H deriving (Show, Eq, Enum, Bounded)
data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Show, Eq, Enum, Bounded)

annotatedBoard :: Board -> [(Coordinate, Square)]
annotatedBoard (Board rows) =
  concat
  $ zipWith (\i (Row squares)->
     zipWith (\j square ->
                (Coordinate j i, square)
             ) [A ..  H]
     squares) [R1 .. R8]
    rows

data Castling = Castling
  { whiteKingSide :: Bool
  , whiteQueenSide :: Bool
  , blackKingSide :: Bool
  , blackQueenSide :: Bool
  } deriving (Show, Eq)

data Coordinate = Coordinate
  { file :: File
  , rank :: Rank
  } deriving (Show, Eq)

data Position = Position
  { board :: Board
  , turn :: Color
  , castling ::  Castling
  , enPassant :: Maybe Coordinate
  , halfMoveClock :: Int
  , fullMoveNumber :: Int
  } deriving (Show, Eq)

-- Assume user puts in relatively well formed data

data StandardMove = StandardMove Piece Coordinate Coordinate deriving (Show, Eq)

data Capture = Capture Piece Coordinate Coordinate deriving (Show, Eq)

data Castle = KingsideCastle | QueensideCastle deriving (Show, Eq)

data EnPassant = EnPassant Coordinate deriving (Show, Eq)

data Promotion = Promotion Coordinate Piece deriving (Show, Eq)

data Move
  = StdMove StandardMove
  | CapMove Capture
  | CastMove Castle
  | PromMove Promotion
  deriving (Show, Eq)

data Game = Game
  { position :: Position
  , previousPosition :: Position
  } deriving (Show, Eq)

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

-- test1
wBoard1 :: Board
wBoard1 =
  Board
    [
      Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Occupied White King],
      Row [Occupied White Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty],
      Row [Empty, Empty,  Occupied White Pawn, Empty, Occupied White Pawn, Empty, Occupied White Pawn, Occupied Black King],
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

