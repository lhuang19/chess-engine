module MoveParser where

import Control.Applicative
import Data.Char qualified as Char
import Data.Functor (($>))
import Data.List qualified as List
import Syntax
import Parser (Parser)
import Parser qualified as P
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import CommonParser

-- moveInputP:: Parser MoveInput
-- moveInputP =
--   MoveInput
--   <$> P.optional (pieceP)
--   <*> P.optional (fileP)
--   <*> P.optional (rankP)
--   <*> coordinateP
--   <*> P.optional (constP "x" ())
--   <*> squareP
--   <*> P.optional (constP "=" ())
--   <*> P.optional pieceP
--   <*> P.optional (constP "e.p." ())


-- candidateCoordinates :: Position -> Piece -> Maybe File -> Maybe Rank -> [Coordinate]
-- candidateCoordinates pos p f r = case
--   Pawn -> 
--   Knight -> knightCandidateCoordinates
--   Bishop -> bishopCandidateCoordinates
--   Rook -> rookCandidateCoordinates
--   Queen -> queenCandidateCoordinates
--   King -> kingCandidateCoordinates



-- moveToStandardMove :: MoveInput -> Position -> Maybe StandardMove
-- moveToStandardMove (MoveInput p f r c x s e p' e') = do
--   let piece = case p of
--         Nothing -> Pawn
--         Just p -> p
--   if x == Nothing && e == Nothing && p' == Nothing && e' == Nothing
--     then
--     case (f, r) of
--       (Just f, Just r) -> Just (StandardMove piece f r c)
      


--     Just (StandardMove (fromJust p) (fromJust f) (fromJust r) c)
--     else Nothing


-- standardMoveP :: Position -> Parser StandardMove
-- standardMoveP = do
--   pos <- P.get
--   move <- moveInputP
--   let piece = case move of
--         MoveInput Nothing _ _ _ _ _ _ _ _ _ -> Pawn
--         MoveInput (Just p) _ _ _ _ _ _ _ _ _ -> p
--   let file = case move of
--         MoveInput _ Nothing _ _ _ _ _ _ _ _ -> Nothing
--         MoveInput _ (Just f) _ _ _ _ _ _ _ _ -> Just f
--   let rank = case move of
--         MoveInput _ _ Nothing _ _ _ _ _ _ _ -> Nothing
--         MoveInput _ _ (Just r) _ _ _ _ _ _ _ -> Just r
--   let to = case move of
--         MoveInput _ _ _ c _ _ _ _ _ _ -> c
--   let capture = case move of
--         MoveInput _ _ _ _ (Just _) _ _ _ _ _ -> True
--         MoveInput _ _ _ _ Nothing _ _ _ _ _ -> False
--   let captured = case move of
--         MoveInput _ _ _ _ _ s _ _ _ _ -> s
--   let promotion = case move of
--         MoveInput _ _ _ _ _ _ _ (Just _) _ _ -> True
--         MoveInput _ _ _ _ _ _ _ Nothing _ _ -> False
--   let promoted = case move of
--         MoveInput _ _ _ _ _ _ _ _ (Just p) _ -> p
--   let enPassant = case move of
--         MoveInput _ _ _ _ _ _ _ _ _ (Just _) -> True
--         MoveInput _ _ _ _ _ _ _ _ _ Nothing -> False
--   let from = case move of
--         MoveInput _ _ _ c _ _ _ _ _ _ -> case c of
--           Coordinate f r -> Coordinate (maybe (fileOf pos piece rank to) id file) (maybe (rankOf pos piece file to) id rank)


  

-- castleP :: Parser Castle
-- castleP =
--   constP "O-O-O" QueenSide
--   <|> constP "O-O" KingSide



  
