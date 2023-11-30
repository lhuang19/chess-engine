module Moves (
  candidateCoordinates,
  pawnMoves,
  knightMoves,
  bishopMoves,
  rookMoves,
  queenMoves,
  kingMoves
) where

import Data.Maybe (maybeToList, isJust)
import Control.Monad ((>=>))
import Data.List qualified as List
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))

import Syntax

validFrom :: Position -> Piece -> Coordinate -> Bool
validFrom (Position b t _ _ _ _) p from =
  isJust $ List.find (==(from, Occupied t p)) (annotatedBoard b)

candidateCoordinates :: Position -> Piece -> Coordinate -> [Coordinate]
candidateCoordinates (Position _ t _ _ _ _) p from = do
  let
    c = case t of
      White -> White
      Black -> Black
  case p of
    Pawn -> pawnMoves c from
    Knight -> knightMoves from
    Bishop -> bishopMoves from
    Rook -> rookMoves from
    Queen -> queenMoves from
    King -> kingMoves from

pawnMoves :: Color -> Coordinate -> [Coordinate]
pawnMoves c (Coordinate f r) = case c of
  White -> case r of
    R2 -> [Coordinate f R3, Coordinate f R4]
    R8 -> error "pawnMoves: white pawn on rank 8"
    _ -> maybeToList $ Coordinate f <$> succRank r
  Black -> case r of
    R7 -> [Coordinate f R6, Coordinate f R5]
    R1 -> error "pawnMoves: black pawn on rank 1"
    _ -> maybeToList $ Coordinate f <$> predRank r

knightMoves :: Coordinate -> [Coordinate]
knightMoves (Coordinate f r) =
  maybeToList (Coordinate <$> predFile2 f <*> predRank r) ++
  maybeToList (Coordinate <$> predFile2 f <*> succRank r) ++
  maybeToList (Coordinate <$> predFile f <*> predRank2 r) ++
  maybeToList (Coordinate <$> predFile f <*> succRank2 r) ++
  maybeToList (Coordinate <$> succFile f <*> predRank2 r) ++
  maybeToList (Coordinate <$> succFile f <*> succRank2 r) ++
  maybeToList (Coordinate <$> succFile2 f <*> predRank r) ++
  maybeToList (Coordinate <$> succFile2 f <*> succRank r)
  where
    succFile2 = succFile >=> succFile
    predFile2 = predFile >=> predFile
    succRank2 = succRank >=> succRank
    predRank2 = predRank >=> predRank

iterateM :: (a -> Maybe a) -> a -> [a]
iterateM f x = x : maybe [] (iterateM f) (f x)

bishopMoves :: Coordinate -> [Coordinate]
bishopMoves (Coordinate f r) =
  filter (\(Coordinate f' r') -> f' /= f && r' /= r) $
  concat
    [ movesInDirection succFile succRank
    , movesInDirection succFile predRank
    , movesInDirection predFile succRank
    , movesInDirection predFile predRank
    ]
  where
    movesInDirection :: (File -> Maybe File) -> (Rank -> Maybe Rank) -> [Coordinate]
    movesInDirection nextFile nextRank =
      map (\(f', r') -> Coordinate f' r') $ zip (iterateM nextFile f) (iterateM nextRank r)


rookMoves :: Coordinate -> [Coordinate]
rookMoves (Coordinate f r) =
  concat
    [ movesInFile succFile
    , movesInFile predFile
    , movesInRank succRank
    , movesInRank predRank
    ]
  where
    movesInFile:: (File -> Maybe File) -> [Coordinate]
    movesInFile nextFile =
      map (`Coordinate` r) $ iterateM nextFile f

    movesInRank :: (Rank -> Maybe Rank) -> [Coordinate]
    movesInRank nextRank =
      map (Coordinate f) $ iterateM nextRank r

queenMoves :: Coordinate -> [Coordinate]
queenMoves c = bishopMoves c ++ rookMoves c

kingMoves :: Coordinate -> [Coordinate]
kingMoves (Coordinate f r) =
  concat
    [ movesInFile succFile
    , movesInFile predFile
    , movesInRank succRank
    , movesInRank predRank
    ]
  where
    movesInFile:: (File -> Maybe File) -> [Coordinate]
    movesInFile nextFile =
      map (`Coordinate` r) $ maybeToList (nextFile f)

    movesInRank :: (Rank -> Maybe Rank) -> [Coordinate]
    movesInRank nextRank =
      map (Coordinate f) $ maybeToList (nextRank r)

test_pawn :: Test
test_pawn =
  "pawn moves" ~:
  TestList
    [ pawnMoves White (Coordinate A R2) ~?= [Coordinate A R3, Coordinate A R4]
    , pawnMoves White (Coordinate A R3) ~?= [Coordinate A R4]
    , pawnMoves Black (Coordinate A R7) ~?= [Coordinate A R6, Coordinate A R5]
    , pawnMoves Black (Coordinate A R6) ~?= [Coordinate A R5]
    ]

test_knight :: Test
test_knight =
  "knight moves" ~:
  TestList
    [ knightMoves (Coordinate A R1) ~?= [Coordinate B R3, Coordinate C R2]
    , knightMoves (Coordinate B R1) ~?= [Coordinate A R3, Coordinate C R3, Coordinate D R2]
    , knightMoves (Coordinate C R1) ~?=
      [
        Coordinate A R2,
        Coordinate B R3,
        Coordinate D R3,
        Coordinate E R2
       ]
    , knightMoves (Coordinate D R3) ~?=
      [
        Coordinate B R2,
        Coordinate B R4,
        Coordinate C R1,
        Coordinate C R5,
        Coordinate E R1,
        Coordinate E R5,
        Coordinate F R2,
        Coordinate F R4
      ]
    ]

test_bishop :: Test
test_bishop =
  "bishop moves" ~:
  TestList
    [ bishopMoves (Coordinate A R1) ~?=
      [
        Coordinate B R2,
        Coordinate C R3,
        Coordinate D R4,
        Coordinate E R5,
        Coordinate F R6,
        Coordinate G R7,
        Coordinate H R8
      ],
      bishopMoves (Coordinate D R4) ~?=
      [
        Coordinate E R5,
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
    [ rookMoves (Coordinate A R1) ~?=
      [
        Coordinate A R2,
        Coordinate A R3,
        Coordinate A R4,
        Coordinate A R5,
        Coordinate A R6,
        Coordinate A R7,
        Coordinate A R8,
        Coordinate B R1,
        Coordinate C R1,
        Coordinate D R1,
        Coordinate E R1,
        Coordinate F R1,
        Coordinate G R1,
        Coordinate H R1
      ],
      rookMoves (Coordinate D R4) ~?=
      [
        Coordinate D R5,
        Coordinate D R6,
        Coordinate D R7,
        Coordinate D R8,
        Coordinate E R4,
        Coordinate F R4,
        Coordinate G R4,
        Coordinate H R4,
        Coordinate D R3,
        Coordinate D R2,
        Coordinate D R1,
        Coordinate C R4,
        Coordinate B R4,
        Coordinate A R4
      ]
    ]


test_all :: IO Counts
test_all = runTestTT $ TestList
  [
    test_pawn,
    test_knight,
    test_bishop,
    test_rook
  ]


