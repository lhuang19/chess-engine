module TestMoves where

import Control.Monad (guard, (>=>))
import Data.Bifunctor (second)
import Data.List qualified as List
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import FENParser (parseFENexn)
import Moves
import Syntax
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, oneof, vector)
import Test.QuickCheck qualified as QC
import Text.Printf (printf)
import Util

sortedValidMoves = List.sort . List.map (show . fst) . validMoves . parseFENexn

test_pawnMove :: Test
test_pawnMove =
  "Pawn push" ~:
    TestList
      [ "Single step forward" ~:
          sortedValidMoves "8/8/8/8/8/4P3/8/8 w - - 0 1"
            ~?= [ "Pawn e3 to e4"
                ],
        "Double step forward" ~:
          sortedValidMoves "8/8/8/8/8/8/2P5/8 w - - 0 1"
            ~?= [ "Pawn c2 to c3",
                  "Pawn c2 to c4"
                ],
        "Capture move" ~:
          sortedValidMoves "8/8/8/2ppp3/3P4/8/8/8 w - - 0 1"
            ~?= [ "Pawn d4 to c5",
                  "Pawn d4 to e5"
                ],
        "En passant left" ~:
          sortedValidMoves "8/8/8/3pPp2/8/8/8/8 w - d6 0 1"
            ~?= [ "Pawn e5 to d6",
                  "Pawn e5 to e6"
                ],
        "En passant right" ~:
          sortedValidMoves "8/8/8/3pPp2/8/8/8/8 w - f6 0 1"
            ~?= [ "Pawn e5 to e6",
                  "Pawn e5 to f6"
                ],
        "Blocked pawn" ~:
          sortedValidMoves "8/8/8/3p4/3P4/8/8/8 w - - 0 1"
            ~?= [],
        "Left edge" ~:
          sortedValidMoves "8/8/8/1p6/P7/8/8/8 w - - 0 1"
            ~?= [ "Pawn a4 to a5",
                  "Pawn a4 to b5"
                ],
        "Right edge" ~:
          sortedValidMoves "8/8/8/6p1/7P/8/8/8 w - - 0 1"
            ~?= [ "Pawn h4 to g5",
                  "Pawn h4 to h5"
                ],
        "Promote capture left" ~:
          sortedValidMoves "4k3/1P6/8/8/1b5b/4b3/3PPP2/3PKP2 w - - 0 1"
            ~?= [ "Pawn b7 to b8 =Bishop",
                  "Pawn b7 to b8 =Knight",
                  "Pawn b7 to b8 =Queen",
                  "Pawn b7 to b8 =Rook"
                ],
        "Promote push" ~:
          sortedValidMoves "q3k3/1P6/8/8/1b5b/4b3/3PPP2/3PKP2 w - - 0 1"
            ~?= [ "Pawn b7 to a8 =Bishop",
                  "Pawn b7 to a8 =Knight",
                  "Pawn b7 to a8 =Queen",
                  "Pawn b7 to a8 =Rook",
                  "Pawn b7 to b8 =Bishop",
                  "Pawn b7 to b8 =Knight",
                  "Pawn b7 to b8 =Queen",
                  "Pawn b7 to b8 =Rook"
                ],
        "Promote capture right" ~:
          sortedValidMoves "q1q1k3/1P6/8/8/1b5b/4b3/3PPP2/3PKP2 w - - 0 1"
            ~?= [ "Pawn b7 to a8 =Bishop",
                  "Pawn b7 to a8 =Knight",
                  "Pawn b7 to a8 =Queen",
                  "Pawn b7 to a8 =Rook",
                  "Pawn b7 to b8 =Bishop",
                  "Pawn b7 to b8 =Knight",
                  "Pawn b7 to b8 =Queen",
                  "Pawn b7 to b8 =Rook",
                  "Pawn b7 to c8 =Bishop",
                  "Pawn b7 to c8 =Knight",
                  "Pawn b7 to c8 =Queen",
                  "Pawn b7 to c8 =Rook"
                ],
        "Promote can't push" ~:
          sortedValidMoves "qrq1k3/1P6/8/8/1b5b/4b3/3PPP2/3PKP2 w - - 0 1"
            ~?= [ "Pawn b7 to a8 =Bishop",
                  "Pawn b7 to a8 =Knight",
                  "Pawn b7 to a8 =Queen",
                  "Pawn b7 to a8 =Rook",
                  "Pawn b7 to c8 =Bishop",
                  "Pawn b7 to c8 =Knight",
                  "Pawn b7 to c8 =Queen",
                  "Pawn b7 to c8 =Rook"
                ],
        "Can't capture promote since pinned" ~:
          sortedValidMoves "2krrr2/4P3/8/8/8/8/8/4K3 w - - 0 1"
            ~?= [ "King e1 to e2"
                ],
        "Can't promote since pinned" ~:
          sortedValidMoves "3r4/3rP2K/3r4/8/8/8/8/8 w - - 0 1"
            ~?= [ "King h7 to g7"
                ]
      ]

test_castling :: Test
test_castling =
  "Castling" ~:
    TestList
      [ "White kingside" ~:
          sortedValidMoves "8/8/8/8/8/8/7r/2rNK2R w K - 0 1"
            ~?= [ "King e1 to f1",
                  "O-O",
                  "Rook h1 to f1",
                  "Rook h1 to g1",
                  "Rook h1 to h2"
                ],
        "White queenside" ~:
          sortedValidMoves "8/8/8/8/8/8/r7/R3KNr1 w Q - 0 1"
            ~?= [ "King e1 to d1",
                  "O-O-O",
                  "Rook a1 to a2",
                  "Rook a1 to b1",
                  "Rook a1 to c1",
                  "Rook a1 to d1"
                ],
        "Black kingside" ~:
          sortedValidMoves "2Rnk2r/7R/8/8/8/8/8/8 b k - 0 1"
            ~?= [ "King e8 to f8",
                  "O-O",
                  "Rook h8 to f8",
                  "Rook h8 to g8",
                  "Rook h8 to h7"
                ],
        "Black queenside" ~:
          sortedValidMoves "r3knR1/R7/8/8/8/8/8/8 b q - 0 1"
            ~?= [ "King e8 to d8",
                  "O-O-O",
                  "Rook a8 to a7",
                  "Rook a8 to b8",
                  "Rook a8 to c8",
                  "Rook a8 to d8"
                ],
        "Blocked by own piece kingside" ~:
          sortedValidMoves "8/8/8/8/8/8/7r/2rNK1NR w K - 0 1"
            ~?= [ "King e1 to f1",
                  "Knight g1 to e2",
                  "Knight g1 to f3",
                  "Knight g1 to h3",
                  "Rook h1 to h2"
                ],
        "Blocked by own piece queenside" ~:
          sortedValidMoves "8/8/8/8/8/8/r7/RN2KNr1 w Q - 0 1"
            ~?= [ "King e1 to d1",
                  "Knight b1 to a3",
                  "Knight b1 to c3",
                  "Knight b1 to d2",
                  "Rook a1 to a2"
                ],
        "Blocked by opponent piece kingside" ~:
          sortedValidMoves "8/8/8/8/8/8/7r/2rNK1bR w K - 0 1"
            ~?= [ "King e1 to f1",
                  "Rook h1 to g1",
                  "Rook h1 to h2"
                ],
        "Blocked by opponent piece queenside" ~:
          sortedValidMoves "8/8/8/8/8/8/r7/Rb2KNr1 w Q - 0 1"
            ~?= [ "King e1 to d1",
                  "Rook a1 to a2",
                  "Rook a1 to b1"
                ],
        "King in check kingside" ~:
          sortedValidMoves "8/8/8/8/8/2b5/7r/3NK2R w K - 0 1"
            ~?= [ "King e1 to f1",
                  "Knight d1 to c3"
                ],
        "King can't castle through f1 check kingside" ~:
          sortedValidMoves "8/8/8/8/8/3b4/7r/2rNK2R w K - 0 1"
            ~?= [ "Rook h1 to f1",
                  "Rook h1 to g1",
                  "Rook h1 to h2"
                ],
        "King can't castle through g1 check kingside" ~:
          sortedValidMoves "8/8/8/8/8/4b3/7r/2rNK2R w K - 0 1"
            ~?= [ "King e1 to f1",
                  "Rook h1 to f1",
                  "Rook h1 to g1",
                  "Rook h1 to h2"
                ],
        "King castles through h1 attack kingside" ~:
          sortedValidMoves "8/8/8/8/8/5b2/7r/2rNK2R w K - 0 1"
            ~?= [ "King e1 to f1",
                  "O-O",
                  "Rook h1 to f1",
                  "Rook h1 to g1",
                  "Rook h1 to h2"
                ],
        "King in check queenside" ~:
          sortedValidMoves "8/8/8/8/8/6b1/r7/R3KN2 w Q - 0 1"
            ~?= [ "King e1 to d1",
                  "Knight f1 to g3"
                ],
        "King can't castle through d1 check queenside" ~:
          sortedValidMoves "8/8/8/8/8/5b2/r7/R3KNr1 w Q - 0 1"
            ~?= [ "Rook a1 to a2",
                  "Rook a1 to b1",
                  "Rook a1 to c1",
                  "Rook a1 to d1"
                ],
        "King can't castle through c1 check queenside" ~:
          sortedValidMoves "8/8/8/8/8/4b3/r7/R3KNr1 w Q - 0 1"
            ~?= [ "King e1 to d1",
                  "Rook a1 to a2",
                  "Rook a1 to b1",
                  "Rook a1 to c1",
                  "Rook a1 to d1"
                ],
        "King castles through b1 attack queenside" ~:
          sortedValidMoves "8/8/8/8/8/3b4/r7/R3KNr1 w Q - 0 1"
            ~?= [ "King e1 to d1",
                  "O-O-O",
                  "Rook a1 to a2",
                  "Rook a1 to b1",
                  "Rook a1 to c1",
                  "Rook a1 to d1"
                ],
        "King castles through a1 attack queenside" ~:
          sortedValidMoves "8/8/8/8/3b4/8/r7/R3KNr1 w Q - 0 1"
            ~?= [ "King e1 to d1",
                  "O-O-O",
                  "Rook a1 to a2",
                  "Rook a1 to b1",
                  "Rook a1 to c1",
                  "Rook a1 to d1"
                ]
      ]

test_knightMove :: Test
test_knightMove =
  "Knight Move" ~:
    TestList
      [ "Standard moves" ~:
          sortedValidMoves "8/8/8/3N4/8/8/8/8 w - - 0 1"
            ~?= [ "Knight d5 to b4",
                  "Knight d5 to b6",
                  "Knight d5 to c3",
                  "Knight d5 to c7",
                  "Knight d5 to e3",
                  "Knight d5 to e7",
                  "Knight d5 to f4",
                  "Knight d5 to f6"
                ],
        "Capture moves" ~:
          sortedValidMoves "8/1p6/1P3p2/3N4/1p3p2/2p1p3/8/8 w - - 0 1"
            ~?= [ "Knight d5 to b4",
                  "Knight d5 to c3",
                  "Knight d5 to c7",
                  "Knight d5 to e3",
                  "Knight d5 to e7",
                  "Knight d5 to f4",
                  "Knight d5 to f6"
                ],
        "Blocked by own pieces" ~:
          sortedValidMoves "2p1p3/1pP1Pp2/1P3P2/1p1N1p2/1Pp1pP2/2P1P3/8/8 w - - 0 1"
            ~?= [],
        "Edge of the board" ~:
          sortedValidMoves "1N6/8/8/8/8/8/8/8 w - - 0 1"
            ~?= [ "Knight b8 to a6",
                  "Knight b8 to c6",
                  "Knight b8 to d7"
                ]
      ]

test_bishopMove :: Test
test_bishopMove =
  "Bishop Move" ~:
    TestList
      [ "Standard moves" ~:
          sortedValidMoves "8/8/8/4B3/8/8/8/8 w - - 0 1"
            ~?= [ "Bishop e5 to a1",
                  "Bishop e5 to b2",
                  "Bishop e5 to b8",
                  "Bishop e5 to c3",
                  "Bishop e5 to c7",
                  "Bishop e5 to d4",
                  "Bishop e5 to d6",
                  "Bishop e5 to f4",
                  "Bishop e5 to f6",
                  "Bishop e5 to g3",
                  "Bishop e5 to g7",
                  "Bishop e5 to h2",
                  "Bishop e5 to h8"
                ],
        "Capture moves" ~:
          sortedValidMoves "8/2p3p1/8/4B3/8/2p3p1/8/8 w - - 0 1"
            ~?= [ "Bishop e5 to c3",
                  "Bishop e5 to c7",
                  "Bishop e5 to d4",
                  "Bishop e5 to d6",
                  "Bishop e5 to f4",
                  "Bishop e5 to f6",
                  "Bishop e5 to g3",
                  "Bishop e5 to g7"
                ],
        "Blocked by own pieces" ~:
          sortedValidMoves "8/3p1p2/3P1P2/3pBp2/3P1P2/8/8/8 w - - 0 1"
            ~?= [],
        "In corner" ~:
          sortedValidMoves "8/8/8/8/8/8/1p6/B7 w - - 0 1"
            ~?= [ "Bishop a1 to b2"
                ]
      ]

test_rookMove :: Test
test_rookMove =
  "Rook Move" ~:
    TestList
      [ "Standard moves" ~:
          sortedValidMoves "8/8/8/8/4R3/8/8/8 w - - 0 1"
            ~?= [ "Rook e4 to a4",
                  "Rook e4 to b4",
                  "Rook e4 to c4",
                  "Rook e4 to d4",
                  "Rook e4 to e1",
                  "Rook e4 to e2",
                  "Rook e4 to e3",
                  "Rook e4 to e5",
                  "Rook e4 to e6",
                  "Rook e4 to e7",
                  "Rook e4 to e8",
                  "Rook e4 to f4",
                  "Rook e4 to g4",
                  "Rook e4 to h4"
                ],
        "Capture moves" ~:
          sortedValidMoves "8/8/8/4p3/4R3/4P3/8/8 w - - 0 1"
            ~?= [ "Rook e4 to a4",
                  "Rook e4 to b4",
                  "Rook e4 to c4",
                  "Rook e4 to d4",
                  "Rook e4 to e5",
                  "Rook e4 to f4",
                  "Rook e4 to g4",
                  "Rook e4 to h4"
                ],
        "No available moves" ~:
          sortedValidMoves "8/8/4p3/3pPp2/3PRP2/4P3/8/8 w - - 0 1"
            ~?= [],
        "Corner and edge positions" ~:
          sortedValidMoves "R7/8/8/8/8/8/8/8 w - - 0 1"
            ~?= [ "Rook a8 to a1",
                  "Rook a8 to a2",
                  "Rook a8 to a3",
                  "Rook a8 to a4",
                  "Rook a8 to a5",
                  "Rook a8 to a6",
                  "Rook a8 to a7",
                  "Rook a8 to b8",
                  "Rook a8 to c8",
                  "Rook a8 to d8",
                  "Rook a8 to e8",
                  "Rook a8 to f8",
                  "Rook a8 to g8",
                  "Rook a8 to h8"
                ]
      ]

test_queenMove :: Test
test_queenMove =
  "Queen Move" ~:
    TestList
      [ "Standard moves" ~:
          sortedValidMoves "8/8/8/4Q3/8/8/8/8 w - - 0 1"
            ~?= [ "Queen e5 to a1",
                  "Queen e5 to a5",
                  "Queen e5 to b2",
                  "Queen e5 to b5",
                  "Queen e5 to b8",
                  "Queen e5 to c3",
                  "Queen e5 to c5",
                  "Queen e5 to c7",
                  "Queen e5 to d4",
                  "Queen e5 to d5",
                  "Queen e5 to d6",
                  "Queen e5 to e1",
                  "Queen e5 to e2",
                  "Queen e5 to e3",
                  "Queen e5 to e4",
                  "Queen e5 to e6",
                  "Queen e5 to e7",
                  "Queen e5 to e8",
                  "Queen e5 to f4",
                  "Queen e5 to f5",
                  "Queen e5 to f6",
                  "Queen e5 to g3",
                  "Queen e5 to g5",
                  "Queen e5 to g7",
                  "Queen e5 to h2",
                  "Queen e5 to h5",
                  "Queen e5 to h8"
                ],
        "Capture moves" ~:
          sortedValidMoves "8/4b1p1/3p4/3pQp2/3P4/6p1/8/8 w - - 0 1"
            ~?= [ "Queen e5 to d5",
                  "Queen e5 to d6",
                  "Queen e5 to e1",
                  "Queen e5 to e2",
                  "Queen e5 to e3",
                  "Queen e5 to e4",
                  "Queen e5 to e6",
                  "Queen e5 to e7",
                  "Queen e5 to f4",
                  "Queen e5 to f5",
                  "Queen e5 to f6",
                  "Queen e5 to g3",
                  "Queen e5 to g7"
                ],
        "Blocked by own pieces" ~:
          sortedValidMoves "8/8/3p1p2/3PnP2/3PQP2/3PPP2/8/8 w - - 0 1"
            ~?= ["Pawn d4 to e5", "Pawn f4 to e5", "Queen e4 to e5"],
        "Corner" ~:
          sortedValidMoves "8/8/8/8/8/r7/1p6/Q1r5 w - - 0 1"
            ~?= [ "Queen a1 to a2",
                  "Queen a1 to a3",
                  "Queen a1 to b1",
                  "Queen a1 to b2",
                  "Queen a1 to c1"
                ]
      ]

test_kingMove :: Test
test_kingMove =
  "King Move" ~:
    TestList
      [ "Standard moves" ~:
          sortedValidMoves "8/8/8/8/4K3/8/8/8 w - - 0 1"
            ~?= [ "King e4 to d3",
                  "King e4 to d4",
                  "King e4 to d5",
                  "King e4 to e3",
                  "King e4 to e5",
                  "King e4 to f3",
                  "King e4 to f4",
                  "King e4 to f5"
                ],
        "Capture moves" ~:
          sortedValidMoves "8/8/8/3p4/3pKp2/3p4/8/8 w - - 0 1"
            ~?= [ "King e4 to d3",
                  "King e4 to d4",
                  "King e4 to d5",
                  "King e4 to e5",
                  "King e4 to f3",
                  "King e4 to f4",
                  "King e4 to f5"
                ],
        "Blocked by own pieces" ~:
          sortedValidMoves "8/8/3p4/3P1p2/3PKP2/3P4/8/8 w - - 0 1"
            ~?= [ "King e4 to e3",
                  "King e4 to f3",
                  "King e4 to f5"
                ],
        "No available moves" ~:
          sortedValidMoves "8/8/3p2p1/3P1p2/3PKP2/3PPP2/8/8 w - - 0 1"
            ~?= [],
        "Cannot move into check" ~:
          sortedValidMoves "8/8/3q4/8/4K3/6q1/8/8 w - - 0 1"
            ~?= [ "King e4 to f5"
                ]
      ]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ test_pawnMove,
        test_castling,
        test_knightMove,
        test_bishopMove,
        test_rookMove,
        test_queenMove,
        test_kingMove
      ]

-- TODO: quickcheck
-- prop_roundtrip_FEN :: Position -> Bool
-- prop_roundtrip_FEN position =
--   parseFEN (posToFEN position) == Right position

-- qc :: IO ()
-- qc = do
--   putStrLn "roundtrip_FEN"
--   QC.quickCheck prop_roundtrip_FEN
