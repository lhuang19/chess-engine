module CommonParser where

import Control.Applicative
import Data.Char qualified as Char
import Data.Functor (($>))
import Data.List qualified as List
import Syntax
import Parser (Parser)
import Parser qualified as P
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))

constP :: Char -> a -> Parser a
constP s x = P.char s $> x

constP' :: Char -> a -> Parser a
constP' s x = P.char' s $> x

pieceP :: Parser Piece
pieceP =
  constP' 'p' Pawn
  <|> constP' 'n' Knight
  <|> constP' 'b' Bishop
  <|> constP' 'r' Rook
  <|> constP' 'q' Queen
  <|> constP' 'k' King

colorPeekP :: Parser Color
colorPeekP = (\c -> if Char.isUpper c then White else Black) <$> P.alpha'

emptySquaresP :: Parser [Square]
emptySquaresP =
  (`replicate` Empty)
  <$> P.filter (\n -> n <= 8 && n >= 1) P.int

occupiedSquareP :: Parser Square
occupiedSquareP = Occupied <$> colorPeekP <*> pieceP

fileP :: Parser File
fileP =
  constP 'a' A
  <|> constP 'b' B
  <|> constP 'c' C
  <|> constP 'd' D
  <|> constP 'e' E
  <|> constP 'f' F
  <|> constP 'g' G
  <|> constP 'h' H

rankP :: Parser Rank
rankP =
  constP '1' R1
  <|> constP '2' R2
  <|> constP '3' R3
  <|> constP '4' R4
  <|> constP '5' R5
  <|> constP '6' R6
  <|> constP '7' R7
  <|> constP '8' R8

coordinateP :: Parser Coordinate
coordinateP = Coordinate <$> fileP <*> rankP
