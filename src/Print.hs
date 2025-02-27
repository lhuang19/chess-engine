module Print
  ( pretty,
    PP (..),
  )
where

import Data.List (intersperse)
import Syntax
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP
import Util

class PP a where
  pp :: a -> Doc

pretty :: (PP a) => a -> String
pretty = PP.render . pp

instance PP Piece where
  pp Pawn = PP.char 'P'
  pp Knight = PP.char 'N'
  pp Bishop = PP.char 'B'
  pp Rook = PP.char 'R'
  pp Queen = PP.char 'Q'
  pp King = PP.char 'K'

instance PP Color where
  pp White = PP.char 'W'
  pp Black = PP.char 'B'

instance PP Square where
  pp Empty = PP.char ' '
  pp (Occupied c p) = case (c, p) of
    (White, Pawn) -> PP.char 'P'
    (White, Knight) -> PP.char 'N'
    (White, Bishop) -> PP.char 'B'
    (White, Rook) -> PP.char 'R'
    (White, Queen) -> PP.char 'Q'
    (White, King) -> PP.char 'K'
    (Black, Pawn) -> PP.char 'p'
    (Black, Knight) -> PP.char 'n'
    (Black, Bishop) -> PP.char 'b'
    (Black, Rook) -> PP.char 'r'
    (Black, Queen) -> PP.char 'q'
    (Black, King) -> PP.char 'k'

instance PP Row where
  pp (Row xs) = PP.text "|" <+> PP.hcat (map (\x -> pp x <+> PP.text "| ") xs)

pad :: Char -> Doc -> Doc
pad c d = PP.text (replicate 2 c) PP.<> d

padBack :: Char -> Doc -> Doc
padBack c d = d PP.<> PP.text (replicate 3 c)

filePrint :: Doc
filePrint = pad ' ' $ PP.hcat (PP.text "  " : map (padBack ' ' . pp) [A, B, C, D, E, F, G, H])

horizontalLine :: Char -> Char -> Doc
horizontalLine c1 c2 = pad ' ' $ PP.hcat (map (\_ -> padBack c1 (PP.char c2)) [0 .. 7] ++ [PP.char c2])

instance PP Board where
  pp (Board xs) =
    PP.vcat $
      concat
        [ [filePrint, horizontalLine '_' '_'],
          intersperse (horizontalLine '-' '+') $
            zipWith (\i x -> PP.int (8 - i) <+> pp x <+> PP.int (8 - i)) [0 ..] xs,
          [horizontalLine '‾' '‾', filePrint, PP.char '\n']
        ]

instance PP Castling where
  pp (Castling wk wq bk bq) = PP.text "Castling" <+> PP.text (show wk) <+> PP.text (show wq) <+> PP.text (show bk) <+> PP.text (show bq)

instance PP File where
  pp A = PP.char 'a'
  pp B = PP.char 'b'
  pp C = PP.char 'c'
  pp D = PP.char 'd'
  pp E = PP.char 'e'
  pp F = PP.char 'f'
  pp G = PP.char 'g'
  pp H = PP.char 'h'

instance PP Rank where
  pp R1 = PP.char '1'
  pp R2 = PP.char '2'
  pp R3 = PP.char '3'
  pp R4 = PP.char '4'
  pp R5 = PP.char '5'
  pp R6 = PP.char '6'
  pp R7 = PP.char '7'
  pp R8 = PP.char '8'

instance PP Coordinate where
  pp (Coordinate f r) = pp f PP.<> pp r

instance PP (Maybe Coordinate) where
  pp Nothing = PP.text "none"
  pp (Just c) = pp c

instance PP Position where
  pp (Position b t c e h f) =
    pp b
      PP.<> PP.char '\n'
      PP.<> PP.text "turn:      "
      <+> pp t
        PP.<> PP.char '\n'
        PP.<> PP.text "castling:  "
      <+> pp c
        PP.<> PP.char '\n'
        PP.<> PP.text "en passant:"
      <+> pp e
        PP.<> PP.char '\n'
        PP.<> PP.text "half moves:"
      <+> PP.int h
        PP.<> PP.char '\n'
        PP.<> PP.text "full moves:"
      <+> PP.int f PP.<> PP.char '\n'
