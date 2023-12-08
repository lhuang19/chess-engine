import Control.Monad (unless)
import FENParser qualified
import Lib
import MoveParser qualified
import Moves qualified
import System.Exit (exitFailure)
import Test.HUnit
import Test.QuickCheck
import TestMoves qualified

main :: IO ()
main = do
  putStrLn "Testing FEN Parser"
  fenResults <- FENParser.test_all
  putStrLn ""

  putStrLn "Testing Moves"
  moveResults <- TestMoves.test_all
  putStrLn ""

  putStrLn "Testing Move Parser"
  moveParserResults <- MoveParser.test_all

  let results = [fenResults, moveResults, moveParserResults]

  unless (all testsPassed results) exitFailure

testsPassed :: Counts -> Bool
testsPassed Counts {errors = 0, failures = 0} = True
testsPassed _ = False