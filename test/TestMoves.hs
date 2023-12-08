module TestMoves where

import Moves
import FENParser (parseFENexn)
import Control.Monad (guard, (>=>))
import Data.Bifunctor (second)
import Data.List qualified as List
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import Syntax
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, oneof, vector)
import Text.Printf (printf)
import Util

sortedValidMoves = List.sort . List.map (show . fst) . validMoves . parseFENexn

test_pawnPromotion :: Test
test_pawnPromotion =
  "Pawn promotion" ~:
    TestList
      [ sortedValidMoves "4k3/1P6/8/8/1b5b/4b3/3PPP2/3PKP2 w - - 0 1" ~?= [
            "Pawn b7 to b8 =Bishop",
            "Pawn b7 to b8 =Knight",
            "Pawn b7 to b8 =Queen",
            "Pawn b7 to b8 =Rook"
        ]
      ]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ test_pawnPromotion
      ]
