module FENParser where

import CommonParser
import Control.Applicative
import Data.Char qualified as Char
import Data.Functor (($>))
import Data.List qualified as List
import Data.Either (isLeft)
import Moves
import Parser (Parser)
import Parser qualified as P
import Syntax
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Util

-- | parsers
rowP :: Parser Row
rowP = 
    P.filter
      (\(Row xs) -> List.length xs == 8)
      (Row . concat <$> many (emptySquaresP <|> (List.singleton <$> occupiedSquareP)))
    <|> P.failP "Invalid row length"

boardP :: Parser Board
boardP =
  Board
    <$> P.filter
      (\xs -> List.length xs == 8)
      (P.sepBy1 rowP (P.char '/'))
    <|> P.failP "Invalid board length"

turnP :: Parser Color
turnP = (\c -> if c == 'w' then White else Black) <$> P.filter (\c -> c == 'w' || c == 'b') P.alpha <|> P.failP "Invalid turn"

castlingP :: Parser Castling
castlingP =
  P.choice
    [ P.string "KQkq" $> Castling True True True True,
      P.string "KQk" $> Castling True True True False,
      P.string "KQq" $> Castling True True False True,
      P.string "KQ" $> Castling True True False False,
      P.string "Kkq" $> Castling True False True True,
      P.string "Kk" $> Castling True False True False,
      P.string "Kq" $> Castling True False False True,
      P.string "K" $> Castling True False False False,
      P.string "Qkq" $> Castling False True True True,
      P.string "Qk" $> Castling False True True False,
      P.string "Qq" $> Castling False True False True,
      P.string "Q" $> Castling False True False False,
      P.string "kq" $> Castling False False True True,
      P.string "k" $> Castling False False True False,
      P.string "q" $> Castling False False False True,
      P.string "-" $> Castling False False False False
    ]
    <|> P.failP "Invalid castling"

enPassantP :: Parser (Maybe Coordinate)
enPassantP =
  P.choice
    [ constP "-" Nothing,
      Just <$> coordinateP
    ]
    <|> P.failP "Invalid en passant"

halfMoveClockP :: Parser Int
halfMoveClockP = P.filter (>= 0) P.int <|> P.failP "Invalid half move clock"

fullMoveNumberP :: Parser Int
fullMoveNumberP = P.filter (>= 0) P.int <|> P.failP "Invalid full move number"

fenP :: Parser Position
fenP =
  Position
    <$> boardP
    <* P.space
    <*> turnP
    <* P.space
    <*> castlingP
    <* P.space
    <*> enPassantP
    <* P.space
    <*> halfMoveClockP
    <* P.space
    <*> fullMoveNumberP
    <* P.eof

parseBoard :: String -> Either P.ParseError Board
parseBoard = P.parse boardP

parseFEN :: String -> Either P.ParseError Position
parseFEN = P.parse fenP

parseFENexn :: String -> Position
parseFENexn fen = case parseFEN fen of Left e -> error (show e); Right p -> p

squareToFEN :: Square -> String
squareToFEN Empty = "1"
squareToFEN (Occupied color piece) = case color of
  Black -> [Char.toLower pieceChar]
  White -> [pieceChar]
  where
    pieceChar :: Char
    pieceChar = case piece of
      Pawn -> 'P'
      Knight -> 'N'
      Bishop -> 'B'
      Rook -> 'R'
      Queen -> 'Q'
      King -> 'K'

rowToFEN :: Row -> String
rowToFEN (Row xs) =
  -- convert squares to FEN
  -- combine empty squares together
  foldr
    ( \x acc ->
        case (x, acc) of
          (Empty, x : xs) | Char.isDigit x -> succ x : xs
          (_, xs) -> squareToFEN x ++ xs
    )
    ""
    xs

boardToFEN :: Board -> String
boardToFEN (Board rs) = List.intercalate "/" $ map rowToFEN rs

turnToFEN :: Color -> String
turnToFEN White = "w"
turnToFEN Black = "b"

castlingToFEN :: Castling -> String
castlingToFEN (Castling wk wq bk bq) =
  (if wk then "K" else "")
    ++ (if wq then "Q" else "")
    ++ (if bk then "k" else "")
    ++ (if bq then "q" else "")
    ++ (if not (wk || wq || bk || bq) then "-" else "")

enPassantToFEN :: Maybe Coordinate -> String
enPassantToFEN Nothing = "-"
enPassantToFEN (Just c) = show c

halfMoveClockToFEN :: Int -> String
halfMoveClockToFEN = show

fullMoveNumberToFEN :: Int -> String
fullMoveNumberToFEN = show

posToFEN :: Position -> String
posToFEN (Position b t c e h f) =
  boardToFEN b
    ++ " "
    ++ turnToFEN t
    ++ " "
    ++ castlingToFEN c
    ++ " "
    ++ enPassantToFEN e
    ++ " "
    ++ halfMoveClockToFEN h
    ++ " "
    ++ fullMoveNumberToFEN f

test_piece :: Test
test_piece =
  "parsing piece" ~:
    TestList
      [ P.parse (many pieceP) "pnbrqk" ~?= Right [Pawn, Knight, Bishop, Rook, Queen, King],
        P.parse pieceP "P" ~?= Right Pawn,
        P.parse pieceP "a" ~?= Left "Invalid piece"
      ]

test_colorPeek :: Test
test_colorPeek =
  "parsing color peek" ~:
    TestList
      [ P.parse colorPeekP "a" ~?= Right Black,
        P.parse colorPeekP "A" ~?= Right White
      ]

test_emptySquares :: Test
test_emptySquares =
  "parsing empty squares" ~:
    TestList
      [ P.parse emptySquaresP "1" ~?= Right [Empty],
        P.parse emptySquaresP "8" ~?= Right [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        P.parse emptySquaresP "9" ~?= Left "Invalid number of empty squares",
        P.parse emptySquaresP "0" ~?= Left "Invalid number of empty squares"
      ]

test_occupiedSquare :: Test
test_occupiedSquare =
  "parsing occupied square" ~:
    TestList
      [ P.parse (many occupiedSquareP) "pnbrqk"
          ~?= Right
            [ Occupied Black Pawn,
              Occupied Black Knight,
              Occupied Black Bishop,
              Occupied Black Rook,
              Occupied Black Queen,
              Occupied Black King
            ],
        P.parse (many occupiedSquareP) "PNBRQK"
          ~?= Right
            [ Occupied White Pawn,
              Occupied White Knight,
              Occupied White Bishop,
              Occupied White Rook,
              Occupied White Queen,
              Occupied White King
            ]
      ]

test_row :: Test
test_row =
  "parsing row" ~:
    TestList
      [ P.parse rowP "3p4"
          ~?= Right
            ( Row
                [ Empty,
                  Empty,
                  Empty,
                  Occupied Black Pawn,
                  Empty,
                  Empty,
                  Empty,
                  Empty
                ]
            ),
        P.parse rowP "3p5" ~?= Left "Invalid row length",
        P.parse rowP "k3pPn1"
          ~?= Right
            ( Row
                [ Occupied Black King,
                  Empty,
                  Empty,
                  Empty,
                  Occupied Black Pawn,
                  Occupied White Pawn,
                  Occupied Black Knight,
                  Empty
                ]
            ),
        P.parse rowP "a" ~?= Left "No parses"
      ]

test_board :: Test
test_board =
  "parsing board" ~:
    TestList
      [ P.parse boardP "7K/P1p1p1p1/2P1P1Pk/6pP/3p2P1/1P6/3P4/8"
          ~?= Right
            ( Board
                [ Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Occupied White King],
                  Row [Occupied White Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty],
                  Row [Empty, Empty, Occupied White Pawn, Empty, Occupied White Pawn, Empty, Occupied White Pawn, Occupied Black King],
                  Row [Empty, Empty, Empty, Empty, Empty, Empty, Occupied Black Pawn, Occupied White Pawn],
                  Row [Empty, Empty, Empty, Occupied Black Pawn, Empty, Empty, Occupied White Pawn, Empty],
                  Row [Empty, Occupied White Pawn, Empty, Empty, Empty, Empty, Empty, Empty],
                  Row [Empty, Empty, Empty, Occupied White Pawn, Empty, Empty, Empty, Empty],
                  Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                ]
            ),
        P.parse boardP "7K/P1p1p1p1/2P1P1Pk/6pP/3p2P1/1P6/3P4/8/8" ~?= Left "No parses"
      ]

test_turn :: Test
test_turn =
  "parsing turn" ~:
    TestList
      [ P.parse turnP "w" ~?= Right White,
        P.parse turnP "b" ~?= Right Black,
        P.parse turnP "a" ~?= Left "No parses"
      ]

test_castling :: Test
test_castling =
  "parsing castling" ~:
    TestList
      [ P.parse castlingP "KQkq" ~?= Right (Castling True True True True),
        P.parse castlingP "-" ~?= Right (Castling False False False False),
        P.parse castlingP "a" ~?= Left "No parses"
      ]

test_enPassant :: Test
test_enPassant =
  "parsing enPassant" ~:
    TestList
      [ P.parse enPassantP "-" ~?= Right Nothing,
        P.parse enPassantP "a1" ~?= Right (Just (Coordinate A R1))
      ]

test_halfMoveClock :: Test
test_halfMoveClock =
  "parsing halfMoveClock" ~:
    TestList
      [ P.parse halfMoveClockP "0" ~?= Right 0,
        P.parse halfMoveClockP "1" ~?= Right 1,
        P.parse halfMoveClockP "a" ~?= Left "No parses",
        P.parse halfMoveClockP "-1" ~?= Left "No parses"
      ]

test_fullMoveNumber :: Test
test_fullMoveNumber =
  "parsing fullMoveNumber" ~:
    TestList
      [ P.parse fullMoveNumberP "0" ~?= Right 0,
        P.parse fullMoveNumberP "a" ~?= Left "No parses",
        P.parse fullMoveNumberP "-1" ~?= Left "No parses"
      ]

test_parseFEN :: Test
test_parseFEN =
  "parsing FEN" ~:
    TestList
      [ P.parse fenP "7K/P1p1p1p1/2P1P1Pk/6pP/3p2P1/1P6/3P4/8 w - - 0 1"
          ~?= Right
            ( Position
                ( Board
                    [ Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Occupied White King],
                      Row [Occupied White Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty],
                      Row [Empty, Empty, Occupied White Pawn, Empty, Occupied White Pawn, Empty, Occupied White Pawn, Occupied Black King],
                      Row [Empty, Empty, Empty, Empty, Empty, Empty, Occupied Black Pawn, Occupied White Pawn],
                      Row [Empty, Empty, Empty, Occupied Black Pawn, Empty, Empty, Occupied White Pawn, Empty],
                      Row [Empty, Occupied White Pawn, Empty, Empty, Empty, Empty, Empty, Empty],
                      Row [Empty, Empty, Empty, Occupied White Pawn, Empty, Empty, Empty, Empty],
                      Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                    ]
                )
                White
                (Castling False False False False)
                Nothing
                0
                1
            ),
        P.parse fenP "7K/P1p1p1p1/2P1P1Pk/6pP/3p2P1/1P6/3P4/8 w - - 0 0" ~?= Left "No parses"
      ]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ test_piece,
        test_colorPeek,
        test_emptySquares,
        test_occupiedSquare,
        test_row,
        test_board,
        test_turn,
        test_castling,
        test_enPassant,
        test_halfMoveClock,
        test_fullMoveNumber,
        test_parseFEN
      ]

-- Helper function to test a parser
prop_roundtrip_FEN :: Position -> Bool
prop_roundtrip_FEN position =
  parseFEN (posToFEN position) == Right position

qc :: IO [QC.Result]
qc =
  sequence
    [ putStrLn "roundtrip_FEN" >> QC.quickCheckResult prop_roundtrip_FEN
    ]
