module Syntax where

import Text.PrettyPrint (Doc, (<+>))
import qualified Text.PrettyPrint as PP

newtype Row = Row [Square] deriving (Show, Eq)
newtype Board = Board [Row] deriving (Show, Eq)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Color = White | Black deriving (Show, Eq)
data Square = Empty | Occupied Color Piece deriving (Show, Eq)

data Castling = Castling
  { whiteKingSide :: Bool
  , whiteQueenSide :: Bool
  , blackKingSide :: Bool
  , blackQueenSide :: Bool
  } deriving (Show, Eq)

data Coordinate = Coordinate
  { file :: Char
  , rank :: Int
  } deriving (Show, Eq)


data Position = Position
  { board :: Board
  , turn :: Color
  , castling ::  Castling
  , enPassant :: Maybe Coordinate
  , halfMoveClock :: Int
  , fullMoveNumber :: Int
  } deriving (Show, Eq)

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


class PP a where
  pp :: a -> Doc

pretty :: PP a => a -> String
pretty = PP.render . pp

instance PP Piece where
  pp Pawn = PP.char 'P'
  pp Knight = PP.char 'N'
  pp Bishop = PP.char 'B'
  pp Rook = PP.char 'R'
  pp Queen = PP.char 'Q'
  pp King = PP.char 'K'

instance PP Color where
  pp White = PP.char 'W'
  pp Black = PP.char 'B'

instance PP Square where
  pp Empty = PP.text "--"
  pp (Occupied c p) = pp c PP.<> pp p

instance PP Row where
  pp (Row xs) = PP.hcat (map (\x -> pp x <+> PP.text "| ") xs)

instance PP Board where
  pp (Board xs) = PP.vcat (map (\x -> pp x <+> PP.char '\n') xs)
