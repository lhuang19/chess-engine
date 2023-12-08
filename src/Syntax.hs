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
    GameCondition (..),
    startingPosition,
  )
where

import Control.Monad (foldM, (>=>))
import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import Test.QuickCheck (Arbitrary (..), elements, oneof, vector)

data Color = White | Black deriving (Show, Eq)

data Square = Empty | Occupied Color Piece deriving (Show, Eq)

newtype Row = Row [Square] deriving (Show, Eq)

newtype Board = Board [Row] deriving (Show, Eq)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

data File = A | B | C | D | E | F | G | H deriving (Show, Eq, Enum, Bounded)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Show, Eq, Enum, Bounded)

data Coordinate = Coordinate
  { file :: File,
    rank :: Rank
  }
  deriving (Eq)

instance Show Coordinate where
  show (Coordinate f r) = map toLower (show f) ++ drop 1 (show r)

data StandardMove = StandardMove
  { piece :: Piece,
    from :: Coordinate,
    to :: Coordinate
  }
  deriving (Eq)

instance Show StandardMove where
  show (StandardMove p from to) = show p ++ " " ++ show from ++ " to " ++ show to

data Castle = Kingside | Queenside deriving (Show, Eq)

data Promotion = Promotion Coordinate Coordinate Piece deriving (Eq)

instance Show Promotion where
  show (Promotion from to p) = "Pawn " ++ show from ++ " to " ++ show to ++ " =" ++ show p

data Move
  = StdMove StandardMove
  | CastMove Castle
  | PromMove Promotion
  deriving (Eq)

instance Show Move where
  show (StdMove m) = show m
  show (CastMove c) = "Castles " ++ show c
  show (PromMove p) = show p

data Castling = Castling
  { whiteKingSide :: Bool,
    whiteQueenSide :: Bool,
    blackKingSide :: Bool,
    blackQueenSide :: Bool
  }
  deriving (Show, Eq)

data Position = Position
  { board :: Board,
    turn :: Color,
    castling :: Castling,
    enPassant :: Maybe Coordinate,
    halfMoveClock :: Int,
    fullMoveNumber :: Int
  }
  deriving (Show, Eq)

data Game
  = Start
  | Game
      { position :: Position,
        lastMove :: Maybe Move,
        prev :: Game
      }
  deriving (Show, Eq)

data GameCondition
  = Normal
  | Check
  | Checkmate
  | Stalemate
  | FiftyMoveDraw

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
        [ Row [Occupied Black Rook, Occupied Black Knight, Occupied Black Bishop, Occupied Black Queen, Occupied Black King, Occupied Black Bishop, Occupied Black Knight, Occupied Black Rook],
          Row [Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn],
          Row [Occupied White Rook, Occupied White Knight, Occupied White Bishop, Occupied White Queen, Occupied White King, Occupied White Bishop, Occupied White Knight, Occupied White Rook]
        ]
    )
    White
    (Castling True True True True)
    Nothing
    0
    1

------------------------------------------------

instance Arbitrary Board where
  arbitrary = Board <$> vector 8

instance Arbitrary Row where
  arbitrary = Row <$> vector 8

instance Arbitrary Piece where
  arbitrary = elements [Pawn, Knight, Bishop, Rook, Queen, King]

instance Arbitrary Color where
  arbitrary = elements [White, Black]

instance Arbitrary Square where
  arbitrary = oneof [return Empty, Occupied <$> arbitrary <*> arbitrary]

instance Arbitrary File where
  arbitrary = elements [A, B, C, D, E, F, G, H]

instance Arbitrary Rank where
  arbitrary = elements [R1, R2, R3, R4, R5, R6, R7, R8]

instance Arbitrary Castling where
  arbitrary = Castling <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Coordinate where
  arbitrary = Coordinate <$> arbitrary <*> arbitrary

instance Arbitrary StandardMove where
  arbitrary = StandardMove <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Castle where
  arbitrary = elements [Kingside, Queenside]

instance Arbitrary Promotion where
  arbitrary = Promotion <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Move where
  arbitrary = oneof [StdMove <$> arbitrary, CastMove <$> arbitrary, PromMove <$> arbitrary]
