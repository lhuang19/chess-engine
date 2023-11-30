module Syntax
  ( pretty,
    wBoard1,
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
    MoveI (..),
    Move (..),
    Game (..),
    PP (..),
    )

where

import Text.PrettyPrint (Doc, (<+>))
import qualified Text.PrettyPrint as PP

newtype Row = Row [Square] deriving (Show, Eq)
newtype Board = Board [Row] deriving (Show, Eq)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Color = White | Black deriving (Show, Eq)
data Square = Empty | Occupied Color Piece deriving (Show, Eq)

data File = A | B | C | D | E | F | G | H deriving (Show, Eq)
data Rank = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Show, Eq)

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

data MoveI = AlegraicNotation
  { pieceI :: Maybe Piece
  , fileI :: Maybe File
  , rankI :: Maybe Rank
  , destinationI :: Maybe Coordinate
  , promotionI :: Maybe Piece
  } deriving (Show, Eq)

data Move = Move
  { piece :: Piece
  , from :: Coordinate
  , to :: Coordinate
  , promotion :: Maybe Piece
  } deriving (Show, Eq)

data Game = Game
  { position :: Position
  , previousPosition :: Position
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

wPosition1 :: Position
wPosition1 =
  Position
    wBoard1
    White
    (Castling False False False False)
    Nothing
    0
    1


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
  pp Empty = PP.text "_"
  pp (Occupied c p) = pp c PP.<> pp p

instance PP Row where
  pp (Row xs) = PP.text "|" <+> PP.hcat (map (\x -> pp x <+> PP.text "| ") xs)

pad :: Char -> Doc -> Doc
pad c d = PP.text (replicate 4 c) PP.<> d

padBack :: Char -> Doc -> Doc
padBack c d = d PP.<> PP.text (replicate 4 c)

filePrint :: Doc
filePrint = pad ' ' $ PP.hcat (map (padBack ' ' . pp) [A, B, C, D, E, F, G, H])

horizontalLine :: Char -> Doc
horizontalLine c = pad c $ PP.hcat (map (\_ -> padBack c (PP.char c)) [0..7])

instance PP Board where
  pp (Board xs) =
    PP.vcat
    $ concat
    [
      [filePrint, horizontalLine '_'],
      zipWith (\i x -> PP.int (8 - i) <+> pp x <+> PP.int (8 - i)) [0..] xs,
      [horizontalLine '‾', filePrint, PP.char '\n']
    ]

instance PP Castling where
  pp (Castling wk wq bk bq) = PP.text "Castling" <+> PP.text (show wk) <+> PP.text (show wq) <+> PP.text (show bk) <+> PP.text (show bq)

instance PP File where
  pp A = PP.char 'a'
  pp B = PP.char 'b'
  pp C = PP.char 'c'
  pp D = PP.char 'd'
  pp E = PP.char 'e'
  pp F = PP.char 'f'
  pp G = PP.char 'g'
  pp H = PP.char 'h'

instance PP Rank where
  pp One = PP.char '1'
  pp Two = PP.char '2'
  pp Three = PP.char '3'
  pp Four = PP.char '4'
  pp Five = PP.char '5'
  pp Six = PP.char '6'
  pp Seven = PP.char '7'
  pp Eight = PP.char '8'

instance PP Coordinate where
  pp (Coordinate f r) = pp f PP.<> pp r

instance PP (Maybe Coordinate) where
  pp Nothing = PP.text "none"
  pp (Just c) = pp c

instance PP Position where
  pp (Position b t c e h f) =
    pp b PP.<> PP.char '\n' PP.<>
    PP.text "turn:      " <+> pp t PP.<> PP.char '\n' PP.<>
    PP.text "castling:  " <+> pp c PP.<> PP.char '\n' PP.<>
    PP.text "en passant:" <+> pp e PP.<> PP.char '\n' PP.<>
    PP.text "half moves:" <+> PP.int h PP.<> PP.char '\n' PP.<>
    PP.text "full moves:" <+> PP.int f PP.<> PP.char '\n'
