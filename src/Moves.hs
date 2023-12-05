module Moves
  ( pawnMoves,
    knightMoves,
    bishopMoves,
    rookMoves,
    queenMoves,
    kingMoves,
    validMoves,
    validCastle,
    test_all,
  )
where

import Control.Monad ((>=>))
import Data.List qualified as List
import Data.Maybe (catMaybes, maybeToList)
import Syntax
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))

validFrom :: Position -> Coordinate -> Piece -> Bool
validFrom (Position b t _ _ _ _) c p = squareAt b c == Occupied t p

-- | pawn
pawnWhiteDir :: Coordinate -> [Dir]
pawnWhiteDir (Coordinate _ r) = do
  let moves = [Dir CannotCapture [SR], Dir MustCapture [PF, SR], Dir MustCapture [SF, SR]]
  if r == R2 --
    then moves ++ [Dir CannotCapture [SR, SR]]
    else moves

pawnDirs :: Coordinate -> Color -> [Dir]
pawnDirs (Coordinate _ r) c = case c of
  White -> pawnWhiteDir (Coordinate A r)
  Black -> map dirOpposite $ pawnWhiteDir (Coordinate A (rankOp r))

pawnMoves :: Color -> Coordinate -> [Coordinate]
pawnMoves color c = coordinateMoveMultiDir (pawnDirs c color) c

pawnValidMoves :: Position -> Coordinate -> [Coordinate]
pawnValidMoves p c =
  if validFrom p c Pawn
    then coordinateMoveMultiDirValid (pawnDirs c (turn p)) p c
    else []

-- | knight
knightDirs :: [Dir]
knightDirs =
  [ Dir CanCapture [PF, PF, PR],
    Dir CanCapture [PF, PF, SR],
    Dir CanCapture [PF, PR, PR],
    Dir CanCapture [PF, SR, SR],
    Dir CanCapture [SF, SF, PR],
    Dir CanCapture [SF, SF, SR],
    Dir CanCapture [SF, PR, PR],
    Dir CanCapture [SF, SR, SR]
  ]

knightMoves :: Coordinate -> [Coordinate]
knightMoves = coordinateMoveMultiDir knightDirs

knightValidMoves :: Position -> Coordinate -> [Coordinate]
knightValidMoves p c =
  if validFrom p c Knight
    then coordinateMoveMultiDirValid knightDirs p c
    else []

-- | bishop
bishopDirs :: [Dir]
bishopDirs =
  [ Dir CanCapture [SF, SR],
    Dir CanCapture [SF, PR],
    Dir CanCapture [PF, SR],
    Dir CanCapture [PF, PR]
  ]

bishopMoves :: Coordinate -> [Coordinate]
bishopMoves = coordinateMovesMultiDir bishopDirs

bishopValidMoves :: Position -> Coordinate -> [Coordinate]
bishopValidMoves p c =
  if validFrom p c Bishop
    then coordinateMovesMultiDirValid bishopDirs p c
    else []

-- | rook
rookDirs :: [Dir]
rookDirs =
  [ Dir CanCapture [SF],
    Dir CanCapture [PF],
    Dir CanCapture [SR],
    Dir CanCapture [PR]
  ]

rookMoves :: Coordinate -> [Coordinate]
rookMoves = coordinateMovesMultiDir rookDirs

rookValidMoves :: Position -> Coordinate -> [Coordinate]
rookValidMoves p c =
  if validFrom p c Rook
    then coordinateMovesMultiDirValid rookDirs p c
    else []

-- | queen
queenDirs :: [Dir]
queenDirs = bishopDirs ++ rookDirs

queenMoves :: Coordinate -> [Coordinate]
queenMoves = coordinateMovesMultiDir queenDirs

queenValidMoves :: Position -> Coordinate -> [Coordinate]
queenValidMoves p c =
  if validFrom p c Queen
    then coordinateMovesMultiDirValid queenDirs p c
    else []

-- | king
kingDirs :: [Dir]
kingDirs =
  [ Dir CanCapture [SF],
    Dir CanCapture [PF],
    Dir CanCapture [SR],
    Dir CanCapture [PR],
    Dir CanCapture [SF, SR],
    Dir CanCapture [SF, PR],
    Dir CanCapture [PF, SR],
    Dir CanCapture [PF, PR]
  ]

kingMoves :: Coordinate -> [Coordinate]
kingMoves = coordinateMoveMultiDir kingDirs

kingValidMoves :: Position -> Coordinate -> [Coordinate]
kingValidMoves p c =
  if validFrom p c King
    then coordinateMoveMultiDirValid kingDirs p c
    else []

validMoves :: Position -> Piece -> Coordinate -> [Coordinate]
validMoves pos p c = case p of
  Pawn -> pawnValidMoves pos c
  Knight -> knightValidMoves pos c
  Bishop -> bishopValidMoves pos c
  Rook -> rookValidMoves pos c
  Queen -> queenValidMoves pos c
  King -> kingValidMoves pos c

coordinateInCheck :: Position -> Coordinate -> Bool
coordinateInCheck pos@(Position b t _ _ _ _) c =
  -- generate all possible capturing moves for the opposite color
  -- check if any of those moves are the coordinate
  -- if so, then the coordinate is in check
  elem c $
    concatMap (\(c', p) -> validMoves pos p c') (piecesByColor (colorOp t) b)

validCastle :: Position -> Castle -> Bool
validCastle pos@(Position b t c _ _ _) castle =
  eligible
    && startingPositionsValid
    && noChecks
    && squaresInBetweenEmpty
  where
    eligible = case (t, castle) of
      (White, Kingside) -> whiteKingSide c
      (White, Queenside) -> whiteQueenSide c
      (Black, Kingside) -> blackKingSide c
      (Black, Queenside) -> blackQueenSide c

    r = if turn pos == White then R1 else R8

    castleSquares = case castle of
      Kingside ->
        [ Coordinate E r,
          Coordinate F r,
          Coordinate G r,
          Coordinate H r
        ]
      Queenside ->
        [ Coordinate E r,
          Coordinate D r,
          Coordinate C r,
          Coordinate B r,
          Coordinate A r
        ]

    startingPositionsValid = case castle of
      Kingside ->
        validFrom pos (Coordinate E r) King
          && validFrom pos (Coordinate H r) Rook
      Queenside ->
        validFrom pos (Coordinate E r) King
          && validFrom pos (Coordinate A r) Rook

    noChecks = not $ any (coordinateInCheck pos) castleSquares

    squaresInBetweenEmpty = all (\c -> squareAt b c == Empty) castleSquares

test_pawn :: Test
test_pawn =
  "pawn moves" ~:
    TestList
      [ pawnMoves White (Coordinate A R2) ~?= [Coordinate A R3, Coordinate B R3, Coordinate A R4],
        pawnMoves White (Coordinate A R3) ~?= [Coordinate A R4, Coordinate B R4],
        pawnMoves Black (Coordinate A R7) ~?= [Coordinate A R6, Coordinate B R6, Coordinate A R5],
        pawnMoves Black (Coordinate A R6) ~?= [Coordinate A R5, Coordinate B R5],
        pawnMoves White (Coordinate D R2) ~?= [Coordinate D R3, Coordinate C R3, Coordinate E R3, Coordinate D R4],
        pawnMoves Black (Coordinate D R7) ~?= [Coordinate D R6, Coordinate E R6, Coordinate C R6, Coordinate D R5]
      ]

test_knight :: Test
test_knight =
  "knight moves" ~:
    TestList
      [ knightMoves (Coordinate A R1) ~?= [Coordinate C R2, Coordinate B R3],
        knightMoves (Coordinate B R1) ~?= [Coordinate A R3, Coordinate D R2, Coordinate C R3],
        knightMoves (Coordinate C R1)
          ~?= [ Coordinate A R2,
                Coordinate B R3,
                Coordinate E R2,
                Coordinate D R3
              ],
        knightMoves (Coordinate D R3)
          ~?= [ Coordinate B R2,
                Coordinate B R4,
                Coordinate C R1,
                Coordinate C R5,
                Coordinate F R2,
                Coordinate F R4,
                Coordinate E R1,
                Coordinate E R5
              ]
      ]

test_bishop :: Test
test_bishop =
  "bishop moves" ~:
    TestList
      [ bishopMoves (Coordinate A R1)
          ~?= [ Coordinate B R2,
                Coordinate C R3,
                Coordinate D R4,
                Coordinate E R5,
                Coordinate F R6,
                Coordinate G R7,
                Coordinate H R8
              ],
        bishopMoves (Coordinate D R4)
          ~?= [ Coordinate E R5,
                Coordinate F R6,
                Coordinate G R7,
                Coordinate H R8,
                Coordinate E R3,
                Coordinate F R2,
                Coordinate G R1,
                Coordinate C R5,
                Coordinate B R6,
                Coordinate A R7,
                Coordinate C R3,
                Coordinate B R2,
                Coordinate A R1
              ]
      ]

test_rook :: Test
test_rook =
  "rook moves" ~:
    TestList
      [ rookMoves (Coordinate A R1)
          ~?= [ Coordinate B R1,
                Coordinate C R1,
                Coordinate D R1,
                Coordinate E R1,
                Coordinate F R1,
                Coordinate G R1,
                Coordinate H R1,
                Coordinate A R2,
                Coordinate A R3,
                Coordinate A R4,
                Coordinate A R5,
                Coordinate A R6,
                Coordinate A R7,
                Coordinate A R8
              ],
        rookMoves (Coordinate D R4)
          ~?= [ Coordinate E R4,
                Coordinate F R4,
                Coordinate G R4,
                Coordinate H R4,
                Coordinate C R4,
                Coordinate B R4,
                Coordinate A R4,
                Coordinate D R5,
                Coordinate D R6,
                Coordinate D R7,
                Coordinate D R8,
                Coordinate D R3,
                Coordinate D R2,
                Coordinate D R1
              ]
      ]

test_queen :: Test
test_queen =
  "queen moves" ~:
    TestList
      [ queenMoves (Coordinate A R1)
          ~?= [ Coordinate B R2,
                Coordinate C R3,
                Coordinate D R4,
                Coordinate E R5,
                Coordinate F R6,
                Coordinate G R7,
                Coordinate H R8,
                Coordinate B R1,
                Coordinate C R1,
                Coordinate D R1,
                Coordinate E R1,
                Coordinate F R1,
                Coordinate G R1,
                Coordinate H R1,
                Coordinate A R2,
                Coordinate A R3,
                Coordinate A R4,
                Coordinate A R5,
                Coordinate A R6,
                Coordinate A R7,
                Coordinate A R8
              ],
        queenMoves (Coordinate D R4)
          ~?= [ Coordinate E R5,
                Coordinate F R6,
                Coordinate G R7,
                Coordinate H R8,
                Coordinate E R3,
                Coordinate F R2,
                Coordinate G R1,
                Coordinate C R5,
                Coordinate B R6,
                Coordinate A R7,
                Coordinate C R3,
                Coordinate B R2,
                Coordinate A R1,
                Coordinate E R4,
                Coordinate F R4,
                Coordinate G R4,
                Coordinate H R4,
                Coordinate C R4,
                Coordinate B R4,
                Coordinate A R4,
                Coordinate D R5,
                Coordinate D R6,
                Coordinate D R7,
                Coordinate D R8,
                Coordinate D R3,
                Coordinate D R2,
                Coordinate D R1
              ]
      ]

test_king :: Test
test_king =
  "king moves" ~:
    TestList
      [ kingMoves (Coordinate A R1) ~?= [Coordinate B R1, Coordinate A R2, Coordinate B R2],
        kingMoves (Coordinate D R4)
          ~?= [ Coordinate E R4,
                Coordinate C R4,
                Coordinate D R5,
                Coordinate D R3,
                Coordinate E R5,
                Coordinate E R3,
                Coordinate C R5,
                Coordinate C R3
              ]
      ]

test_position1 :: Position
test_position1 =
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

test_position2 :: Position
test_position2 =
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
    Black
    (Castling True True True True)
    Nothing
    0
    1

-- test_position3 :: Position
-- test_position3 = Position
--   (Board
--     [ Row [Occupied White Rook, Occupied White Knight, Occupied White Bishop, Occupied White Queen, Occupied White King, Occupied White Bishop, Occupied White Knight, Occupied White Rook]

test_validPawn :: Test
test_validPawn =
  "valid pawn moves" ~:
    TestList
      [ pawnValidMoves test_position1 (Coordinate A R2) ~?= [Coordinate A R3, Coordinate A R4],
        pawnValidMoves test_position1 (Coordinate D R2) ~?= [Coordinate D R3, Coordinate D R4],
        pawnValidMoves test_position2 (Coordinate A R7) ~?= [Coordinate A R6, Coordinate A R5],
        pawnValidMoves test_position2 (Coordinate D R7) ~?= [Coordinate D R6, Coordinate D R5],
        pawnValidMoves test_position1 (Coordinate D R3) ~?= []
      ]

test_validKnight :: Test
test_validKnight =
  "valid knight moves" ~:
    TestList
      [ knightValidMoves test_position1 (Coordinate B R1)
          ~?= [ Coordinate A R3,
                Coordinate C R3
              ],
        knightValidMoves test_position1 (Coordinate C R1) ~?= [],
        knightValidMoves test_position1 (Coordinate G R1)
          ~?= [ Coordinate F R3,
                Coordinate H R3
              ],
        knightValidMoves test_position2 (Coordinate B R8)
          ~?= [ Coordinate A R6,
                Coordinate C R6
              ],
        knightValidMoves test_position2 (Coordinate C R8) ~?= [],
        knightValidMoves test_position2 (Coordinate G R8)
          ~?= [ Coordinate F R6,
                Coordinate H R6
              ],
        knightValidMoves test_position1 (Coordinate G R8) ~?= []
      ]

test_validBishop :: Test
test_validBishop =
  "valid bishop moves" ~:
    TestList
      [ bishopValidMoves test_position1 (Coordinate C R1) ~?= [],
        bishopValidMoves test_position1 (Coordinate F R1) ~?= [],
        bishopValidMoves test_position2 (Coordinate C R8) ~?= [],
        bishopValidMoves test_position2 (Coordinate F R8) ~?= []
      ]

test_validRook :: Test
test_validRook =
  "valid rook moves" ~:
    TestList
      [ rookValidMoves test_position1 (Coordinate A R1) ~?= [],
        rookValidMoves test_position1 (Coordinate H R1) ~?= [],
        rookValidMoves test_position2 (Coordinate A R8) ~?= [],
        rookValidMoves test_position2 (Coordinate H R8) ~?= []
      ]

test_validQueen :: Test
test_validQueen =
  "valid queen moves" ~:
    TestList
      [ queenValidMoves test_position1 (Coordinate D R1) ~?= [],
        queenValidMoves test_position2 (Coordinate D R8) ~?= []
      ]

test_validKing :: Test
test_validKing =
  "valid king moves" ~:
    TestList
      [ kingValidMoves test_position1 (Coordinate E R1) ~?= [],
        kingValidMoves test_position2 (Coordinate E R8) ~?= []
      ]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ test_pawn,
        test_knight,
        test_bishop,
        test_rook,
        test_queen,
        test_king,
        test_validPawn,
        test_validKnight,
        test_validBishop,
        test_validRook,
        test_validQueen,
        test_validKing
      ]
