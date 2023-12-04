module MoveParser where

import CommonParser
import Control.Applicative
import Data.Char qualified as Char
import Data.Functor (($>))
import Data.List qualified as List
import Parser (Parser)
import Parser qualified as P
import Syntax
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))

standardMoveP :: Parser StandardMove
standardMoveP = StandardMove <$> pieceP <*> coordinateP <*> coordinateP <* P.eof

castleP :: Parser Castle
castleP =
  constP "O-O" Kingside <* P.eof
  <|> constP "O-O-O" Queenside <* P.eof

promotionP :: Parser Promotion
promotionP =
  Promotion
  <$ pawnP
  <*> coordinateP
  <*> coordinateP
  <*> promotableP
  <* P.eof

moveP :: Parser Move
moveP =
  PromMove <$> promotionP
  <|> CastMove <$> castleP
  <|> StdMove <$> standardMoveP

parseMove :: String -> Either P.ParseError Move
parseMove = P.parse moveP

test_standardMove :: Test
test_standardMove =
  "parsing standard move"
  ~: TestList
  [ P.parse standardMoveP "pe2e4" ~?= Right (StandardMove Pawn (Coordinate E R2) (Coordinate E R4)),
    P.parse standardMoveP "Nc3d5" ~?= Right (StandardMove Knight (Coordinate C R3) (Coordinate D R5)),
    P.parse standardMoveP "Bf1c4" ~?= Right (StandardMove Bishop (Coordinate F R1) (Coordinate C R4)),
    P.parse standardMoveP "Rg1g8" ~?= Right (StandardMove Rook (Coordinate G R1) (Coordinate G R8)),
    P.parse standardMoveP "Qd1h5" ~?= Right (StandardMove Queen (Coordinate D R1) (Coordinate H R5)),
    P.parse standardMoveP "Ke1e2" ~?= Right (StandardMove King (Coordinate E R1) (Coordinate E R2)),
    P.parse standardMoveP "pe2e4a" ~?= Left "No parses"
  ]

test_castle :: Test
test_castle =
  "parsing castle"
  ~: TestList
  [ P.parse castleP "O-O" ~?= Right Kingside,
    P.parse castleP "O-O-O" ~?= Right Queenside,
    P.parse castleP "O-O-Oa" ~?= Left "No parses"
  ]

test_promotion :: Test
test_promotion =
  "parsing promotion"
  ~: TestList
  [ P.parse promotionP "pe7e8q" ~?= Right (Promotion (Coordinate E R7) (Coordinate E R8) Queen),
    P.parse promotionP "pe7e8n" ~?= Right (Promotion (Coordinate E R7) (Coordinate E R8) Knight),
    P.parse promotionP "pe7e8b" ~?= Right (Promotion (Coordinate E R7) (Coordinate E R8) Bishop),
    P.parse promotionP "pe7e8r" ~?= Right (Promotion (Coordinate E R7) (Coordinate E R8) Rook),
    P.parse promotionP "pe7e8k" ~?= Left "No parses"
  ]


test_all :: IO Counts
test_all = runTestTT $ TestList
  [ test_standardMove,
    test_castle,
    test_promotion
  ]


