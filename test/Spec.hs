import Test.HUnit
import Test.QuickCheck

import Lib

import qualified FENParser
import qualified Moves
import qualified MoveParser


main :: IO ()
main = do 
    putStrLn "Testing FEN Parser"
    _ <- FENParser.test_all
    putStrLn ""


    putStrLn "Testing Moves"
    _ <- Moves.test_all
    putStrLn ""

    putStrLn "Testing Move Parser"
    _ <- MoveParser.test_all
    putStrLn ""

    return ()
