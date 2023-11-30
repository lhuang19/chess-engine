module Main where

import Lib
import FENParser
import Syntax
 
main :: IO ()
main = do
    -- putStrLn "Enter a string:"
    -- input <- getLine
    -- putStrLn $ "You entered: " ++ input
    putStrLn $ "Testing Syntax"
    putStrLn $ "7K/P1p1p1p1/2P1P1Pk/6pP/3p2P1/1P6/3P4/8 w - - 0 1"
    putStrLn $ "7K/P1p1p1p1/2P1P1Pk/6pP/3p2P1/1P6/3P4/8 w - - 0 1"
    case parseBoard "7K/P1p1p1p1/2P1P1Pk/6pP/3p2P1/1P6/3P4/8" of
        Left err -> putStrLn $ "Error: " ++ err
        Right fen -> putStrLn $ pretty fen
    case parseFEN "7K/P1p1p1p1/2P1P1Pk/6pP/3p2P1/1P6/3P4/8 w - - 0 1" of
        Left err -> putStrLn $ "Error: " ++ err
        Right fen -> putStrLn $ pretty fen

    putStrLn $ "end"
