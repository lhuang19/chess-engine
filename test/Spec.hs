import FENParser qualified
import Lib
import MoveParser qualified
import Moves qualified
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Testing FEN Parser"
  _ <- FENParser.test_all
  putStrLn ""

  -- putStrLn "Testing Moves"
  -- _ <- Moves.test_all
  -- putStrLn ""

  putStrLn "Testing Move Parser"
  _ <- MoveParser.test_all
  putStrLn ""

  return ()
