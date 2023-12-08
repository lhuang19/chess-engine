import Control.Monad (unless)
import FENParser qualified
import Lib
import MoveParser qualified
import Moves qualified
import System.Exit (exitFailure)
import Test.HUnit
import Test.QuickCheck (isSuccess)
import TestMoves qualified

main :: IO ()
main = do
  putStrLn "Testing FEN Parser"
  fenUnitResults <- FENParser.test_all
  fenQCResults <- FENParser.qc
  putStrLn ""

  putStrLn "Testing Moves"
  moveUnitResults <- TestMoves.test_all
  moveQCResults <- TestMoves.qc
  putStrLn ""

  putStrLn "Testing Move Parser"
  moveParserUnitResults <- MoveParser.test_all

  let unitResults = [fenUnitResults, moveUnitResults, moveParserUnitResults]
  let qcResults = fenQCResults ++ moveQCResults

  unless (all testsPassed unitResults && all isSuccess qcResults) exitFailure

testsPassed :: Counts -> Bool
testsPassed Counts {errors = 0, failures = 0} = True
testsPassed _ = False