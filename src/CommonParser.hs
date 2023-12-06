module CommonParser where

import Control.Applicative
import Data.Char qualified as Char
import Data.Functor (($>))
import Data.List qualified as List
import Parser (Parser)
import Parser qualified as P
import Syntax
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Util

constP :: String -> a -> Parser a
constP s x = P.string s $> x

constCharP :: Char -> a -> Parser a
constCharP s x = P.char s $> x

constCharP' :: Char -> a -> Parser a
constCharP' s x = P.char' s $> x

pawnP :: Parser Piece
pawnP = constCharP' 'p' Pawn

knightP :: Parser Piece
knightP = constCharP' 'n' Knight

bishopP :: Parser Piece
bishopP = constCharP' 'b' Bishop

rookP :: Parser Piece
rookP = constCharP' 'r' Rook

queenP :: Parser Piece
queenP = constCharP' 'q' Queen

kingP :: Parser Piece
kingP = constCharP' 'k' King

unpromotableP :: Parser Piece
unpromotableP = pawnP <|> kingP

promotableP :: Parser Piece
promotableP = knightP <|> bishopP <|> rookP <|> queenP

pieceP :: Parser Piece
pieceP = unpromotableP <|> promotableP

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
  constCharP 'a' A
    <|> constCharP 'b' B
    <|> constCharP 'c' C
    <|> constCharP 'd' D
    <|> constCharP 'e' E
    <|> constCharP 'f' F
    <|> constCharP 'g' G
    <|> constCharP 'h' H

rankP :: Parser Rank
rankP =
  constCharP '1' R1
    <|> constCharP '2' R2
    <|> constCharP '3' R3
    <|> constCharP '4' R4
    <|> constCharP '5' R5
    <|> constCharP '6' R6
    <|> constCharP '7' R7
    <|> constCharP '8' R8

coordinateP :: Parser Coordinate
coordinateP = Coordinate <$> fileP <*> rankP
