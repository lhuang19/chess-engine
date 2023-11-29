module FENParser where

import Control.Applicative
import Data.Char (isAlpha, isAlphaNum)
import Data.Char qualified as Char
import Data.Functor (($>))
import Syntax
import Parser (Parser)
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))


wsP :: Parser a -> Parser a
wsP p = p <* many P.space

constP :: String -> a -> Parser a
constP s x = wsP (P.string s $> x)

-- | Parse piece
pieceP :: Parser Piece
pieceP =
  constP "p" Pawn
  <|> constP "k" Knight
  <|> constP "b" Bishop
  <|> constP "r" Rook
  <|> constP "q" Queen
  <|> constP "k" King

rowP :: Parser Row
rowP = undefined


boardP :: Parser Board
boardP = Board <$> many rowP

turnP :: Parser Color
turnP = undefined

castlingP :: Parser Castling
castlingP = undefined

enPassantP :: Parser (Maybe Coordinate)
enPassantP = undefined

halfMoveClockP :: Parser Int
halfMoveClockP = P.int

fullMoveClockP :: Parser Int
fullMoveClockP = P.int

fenP :: Parser Position
fenP = Position <$> boardP <*> turnP <*> castlingP <*> enPassantP <*> halfMoveClockP <*> fullMoveClockP

parseFEN :: String -> Either P.ParseError Position
parseFEN = P.parse fenP




  
