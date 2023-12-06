module Moves
  ( validMoves,
    makeMove,
  )
where

import Control.Monad (guard, (>=>))
import Data.Bifunctor (second)
import Data.List qualified as List
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import Syntax
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, oneof, vector)
import Text.Printf (printf)
import Util

-- NOTE:
-- "candidate moves" do not take into account the rule that you can't move into check
-- "valid moves" do take into account this rule, and are actually playable

-- Properly update all the turn-related fields in the position record
-- The first param is pawnMoveOrCapture, denoting whether either
-- a pawn has just been moved or a piece has just been captured
changeTurn :: Bool -> Position -> Position
changeTurn pawnMoveOrCapture pos =
  pos
    { turn = flipColor $ turn pos,
      enPassant = Nothing,
      halfMoveClock = if pawnMoveOrCapture then 0 else succ $ halfMoveClock pos,
      fullMoveNumber = (if turn pos == Black then succ else id) (fullMoveNumber pos)
    }

-- Pawn moves are the most complicated (forward, double forward, capture diagonal, EN PASSANT 😳)
-- Naive means ignoring promotion rules
pawnCandidateMovesNaive :: Position -> Coordinate -> [(Coordinate, Position)]
pawnCandidateMovesNaive pos@(Position oldBoard color _ _ _ _) from@(Coordinate f r) =
  catMaybes
    [ oneStep,
      twoStep,
      capture predFile,
      capture succFile
    ]
  where
    rankMove = if color == White then succRank else predRank

    oneStep :: Maybe (Coordinate, Position)
    oneStep = do
      toRank <- rankMove r
      let to = Coordinate f toRank
      guard (isSquareEmpty oldBoard to)
      let newBoard = updateBoardSimpleMove oldBoard from to
      return (to, changeTurn True $ pos {board = newBoard})

    twoStep :: Maybe (Coordinate, Position)
    twoStep = do
      guard (color == White && r == R2 || color == Black && r == R7) -- start rank
      guard (isJust oneStep)
      hopOverRank <- rankMove r
      toRank <- rankMove hopOverRank
      let to = Coordinate f toRank
      guard (isSquareEmpty oldBoard to)
      let newBoard = updateBoardSimpleMove oldBoard from to
      let enPassantVulnerability = Coordinate f hopOverRank
      return (to, (changeTurn True pos) {board = newBoard, enPassant = Just enPassantVulnerability})

    capture :: FileMove -> Maybe (Coordinate, Position)
    capture fileMove = do
      toFile <- fileMove f
      toRank <- rankMove r
      let to = Coordinate toFile toRank
      let isEnPassant = enPassant pos == Just to
      guard (isSquareOccupied oldBoard to || isEnPassant)
      let enPassantVictim = Coordinate toFile r
      let enPassantKill = if isEnPassant then updateBoard enPassantVictim Empty else id
      let newBoard = enPassantKill $ updateBoardSimpleMove oldBoard from to
      return (to, changeTurn True $ pos {board = newBoard})

-- Candidate pawn moves (taking into account promotion rules)
pawnCandidateMoves :: Position -> Coordinate -> [(Move, Position)]
pawnCandidateMoves pos from =
  let naiveMoves = pawnCandidateMovesNaive pos from
   in let color = turn pos
       in concatMap
            ( \(to@(Coordinate _ toRank), newPos) ->
                if toRank /= R1 && toRank /= R8
                  then [(StdMove (StandardMove Pawn from to), newPos)]
                  else
                    map
                      ( \p ->
                          ( PromMove (Promotion from to p),
                            newPos {board = updateBoard to (Occupied color p) (board newPos)}
                          )
                      )
                      promotionPieces
            )
            naiveMoves

-- Simple non-pawn step moves, transform a list of coordinates to candidate moves
stepMoves :: Position -> Piece -> Coordinate -> [CoordinateMove] -> [(Move, Position)]
stepMoves pos piece from ms =
  map
    ( \to ->
        let oldBoard = board pos
         in let newBoard = updateBoardSimpleMove oldBoard from to
             in let isCapture = isSquareOccupied oldBoard to
                 in (stdMove piece from to, changeTurn isCapture $ pos {board = newBoard})
    )
    $ mapMaybe (stepCoordinate pos from) ms

-- Candidate knight moves
knightCandidateMoves :: Position -> Coordinate -> [(Move, Position)]
knightCandidateMoves pos c =
  stepMoves
    pos
    Knight
    c
    [ (succFile >=> succFile, succRank),
      (succFile >=> succFile, predRank),
      (predFile >=> predFile, succRank),
      (predFile >=> predFile, predRank),
      (succFile, succRank >=> succRank),
      (succFile, predRank >=> predRank),
      (predFile, succRank >=> succRank),
      (predFile, predRank >=> predRank)
    ]

-- Candidate king moves (other than castling)
kingCandidateMoves :: Position -> Coordinate -> [(Move, Position)]
kingCandidateMoves pos c =
  map
    (Data.Bifunctor.second disableCastling)
    (stepMoves pos King c directions)
  where
    disableCastling :: Position -> Position
    disableCastling p =
      let currentCastling = castling p
          newCastling = case turn p of
            White -> currentCastling {whiteKingSide = False, whiteQueenSide = False}
            Black -> currentCastling {blackKingSide = False, blackQueenSide = False}
       in p {castling = newCastling}

    directions =
      [ (succFile, succRank),
        (succFile, predRank),
        (predFile, succRank),
        (predFile, predRank),
        (Just, succRank),
        (Just, predRank),
        (succFile, Just),
        (predFile, Just)
      ]

-- Simple non-pawn sliding moves (can greedily apply the move as many times as possible),
-- transform a list of coordinates to candidate moves
slideMoves :: Position -> Piece -> Coordinate -> [CoordinateMove] -> [(Move, Position)]
slideMoves pos piece from ms =
  map
    ( \to ->
        let oldBoard = board pos
         in let newBoard = updateBoardSimpleMove oldBoard from to
             in let isCapture = isSquareOccupied oldBoard to
                 in (stdMove piece from to, changeTurn isCapture $ pos {board = newBoard})
    )
    $ concatMap (reachableCoordinates pos from) ms

-- Candidate bishop moves
bishopCandidateMoves :: Position -> Coordinate -> [(Move, Position)]
bishopCandidateMoves pos c =
  slideMoves
    pos
    Bishop
    c
    [ (succFile, succRank),
      (succFile, predRank),
      (predFile, succRank),
      (predFile, predRank)
    ]

-- Candidate rook moves (other than castling)
rookCandidateMoves :: Position -> Coordinate -> [(Move, Position)]
rookCandidateMoves pos c =
  map
    (Data.Bifunctor.second updateCastling)
    (slideMoves pos Rook c directions)
  where
    updateCastling :: Position -> Position
    updateCastling newPos = newPos {castling = coordMapCastling c (castling newPos)}

    coordMapCastling :: Coordinate -> Castling -> Castling
    coordMapCastling (Coordinate A R1) cast = cast {whiteQueenSide = False}
    coordMapCastling (Coordinate H R1) cast = cast {whiteKingSide = False}
    coordMapCastling (Coordinate A R8) cast = cast {blackQueenSide = False}
    coordMapCastling (Coordinate H R8) cast = cast {blackKingSide = False}
    coordMapCastling _ cast = cast

    directions =
      [ (Just, succRank),
        (Just, predRank),
        (succFile, Just),
        (predFile, Just)
      ]

-- Candidate queen moves
queenCandidateMoves :: Position -> Coordinate -> [(Move, Position)]
queenCandidateMoves pos c =
  slideMoves
    pos
    Queen
    c
    [ (succFile, succRank),
      (succFile, predRank),
      (predFile, succRank),
      (predFile, predRank),
      (Just, succRank),
      (Just, predRank),
      (succFile, Just),
      (predFile, Just)
    ]

-- Candidate moves (other than castling)
candidateNonCastlingMoves :: Position -> Coordinate -> [(Move, Position)]
candidateNonCastlingMoves pos@(Position board color _ _ _ _) from = case squareAt board from of
  Empty -> []
  Occupied color piece
    | color /= turn pos -> []
    | otherwise -> case piece of
        Pawn -> pawnCandidateMoves pos from
        Knight -> knightCandidateMoves pos from
        Bishop -> bishopCandidateMoves pos from
        Rook -> rookCandidateMoves pos from
        Queen -> queenCandidateMoves pos from
        King -> kingCandidateMoves pos from

-- Check if opponent can capture our king after this move
-- (i.e. whether this is a move into check)
moveIntoCheck :: Position -> (Move, Position) -> Bool
moveIntoCheck (Position oldBoard color _ _ _ _) (_, newPos) =
  -- generate all possible capturing moves for the opposite color
  -- check if any of moves would capture our king
  -- if so, this is a move into check
  any
    ( \(move, _) ->
        let coord = moveToCoordinate color move
         in squareAt oldBoard coord == Occupied color King
    )
    $ concatMap (candidateNonCastlingMoves newPos . fst)
    $ piecesByColor (turn newPos) (board newPos)

-- Like candidate moves, but with moves into check filtered out
validNonCastlingMoves :: Position -> Coordinate -> [(Move, Position)]
validNonCastlingMoves pos from =
  filter (not . moveIntoCheck pos) $
    candidateNonCastlingMoves pos from

-- Checks if any of the given coordinates are in check
anyCoordinatesInCheck :: Position -> [Coordinate] -> Bool
anyCoordinatesInCheck pos@(Position b t _ _ _ _) coords =
  -- generate all possible capturing moves for the opposite color
  -- check if any of those moves are the coordinate
  -- if so, then the coordinate is in check
  let attackedCoords =
        map (moveToCoordinate t . fst) $
          concatMap (\(c', _) -> candidateNonCastlingMoves pos c') $
            piecesByColor (flipColor t) b
   in any (`elem` attackedCoords) coords

isValidCastle :: Position -> Castle -> Bool
isValidCastle pos@(Position b t c _ _ _) castle =
  eligible
    && noChecks
    && squaresInBetweenEmpty
  where
    eligible = case (t, castle) of
      (White, Kingside) -> whiteKingSide c
      (White, Queenside) -> whiteQueenSide c
      (Black, Kingside) -> blackKingSide c
      (Black, Queenside) -> blackQueenSide c

    r = if turn pos == White then R1 else R8

    noChecks = not $ anyCoordinatesInCheck pos $ case castle of
      Kingside ->
        [ Coordinate E r,
          Coordinate F r,
          Coordinate G r
        ]
      Queenside ->
        [ Coordinate C r,
          Coordinate D r,
          Coordinate E r
        ]

    squaresInBetweenEmpty = all (\c -> squareAt b c == Empty) $ case castle of
      Kingside ->
        [ Coordinate F r,
          Coordinate G r
        ]
      Queenside ->
        [ Coordinate B r,
          Coordinate C r,
          Coordinate D r
        ]

validCastleMoves :: Position -> [(Move, Position)]
validCastleMoves pos@(Position b t _ _ _ _) =
  let rank = if t == White then R1 else R8
   in [ let newBoard =
              updateBoard (Coordinate E rank) Empty $
                updateBoard (Coordinate A rank) Empty $
                  updateBoard (Coordinate C rank) (Occupied t King) $
                    updateBoard (Coordinate D rank) (Occupied t Rook) b
         in (CastMove Queenside, changeTurn False $ pos {board = newBoard})
        | isValidCastle pos Queenside
      ]
        ++ [ let newBoard =
                   updateBoard (Coordinate E rank) Empty $
                     updateBoard (Coordinate H rank) Empty $
                       updateBoard (Coordinate G rank) (Occupied t King) $
                         updateBoard (Coordinate F rank) (Occupied t Rook) b
              in (CastMove Kingside, changeTurn False $ pos {board = newBoard})
             | isValidCastle pos Kingside
           ]

-- All valid moves in the position
validMoves :: Position -> [(Move, Position)]
validMoves pos =
  validCastleMoves pos
    ++ concatMap
      (candidateNonCastlingMoves pos . fst)
      (piecesByColor (turn pos) (board pos))

-- Validate `from` coordinate of move, for more informative error feedback
validateMoveFrom :: Position -> Coordinate -> Piece -> Either String ()
validateMoveFrom pos from piece =
  case squareAt (board pos) from of
    Empty -> Left $ printf "No piece found at %s." (show from)
    Occupied foundColor foundPiece ->
      if foundColor /= turn pos
        then Left "You can't move your opponent's piece."
        else
          if piece /= foundPiece
            then Left $ printf "You don't have a %s on %s." (show foundPiece) (show from)
            else Right ()

-- Validate `to` coordinate of move, for more informative error feedback
validateMoveTo :: Position -> Coordinate -> Either String ()
validateMoveTo pos to =
  case squareAt (board pos) to of
    Empty -> Right ()
    Occupied foundColor _ ->
      if foundColor == turn pos
        then Left $ printf "You can't capture your own piece on %s." (show to)
        else Right ()

makeMove :: Position -> Move -> Either String Position
makeMove pos move@(StdMove (StandardMove piece from to)) =
  do
    validateMoveFrom pos from piece
    validateMoveTo pos to
    case List.find ((== move) . fst) (validNonCastlingMoves pos from) of
      Just (_, newPos) -> Right newPos
      Nothing ->
        Left $
          printf
            "You can't move your %s from %s to %s"
            (show piece)
            (show from)
            (show to)
makeMove pos move@(PromMove (Promotion from to piece)) =
  do
    validateMoveFrom pos from Pawn
    validateMoveTo pos to
    case List.find ((== move) . fst) (validNonCastlingMoves pos from) of
      Just (_, newPos) -> Right newPos
      Nothing ->
        Left $
          printf
            "Your can't promote your pawn to a %s from %s to %s"
            (show piece)
            (show from)
            (show to)
makeMove pos move@(CastMove castle) =
  case List.find ((== move) . fst) (validCastleMoves pos) of
    Just (_, newPos) -> Right newPos
    Nothing -> Left $ printf "You can't castle" (show castle)

------------------------------------------------

instance Arbitrary Position where
  arbitrary = do
    steps <- choose (0, 30) -- Decide how many steps to apply
    applyMoves startingPosition steps

-- Function to recursively apply a random valid move
applyMoves :: Position -> Int -> Gen Position
applyMoves pos 0 = return pos
applyMoves pos n = do
  let moves = validMoves pos
  if null moves
    then return pos -- No more valid moves
    else do
      (move, newPos) <- elements moves
      applyMoves newPos (n - 1)

test_position1 :: Position
test_position1 =
  Position
    ( Board
        [ Row [Occupied White Rook, Occupied White Knight, Occupied White Bishop, Occupied White Queen, Occupied White King, Occupied White Bishop, Occupied White Knight, Occupied White Rook],
          Row [Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn],
          Row [Occupied Black Rook, Occupied Black Knight, Occupied Black Bishop, Occupied Black Queen, Occupied Black King, Occupied Black Bishop, Occupied Black Knight, Occupied Black Rook]
        ]
    )
    White
    (Castling True True True True)
    Nothing
    0
    1

test_position2 :: Position
test_position2 =
  Position
    ( Board
        [ Row [Occupied White Rook, Occupied White Knight, Occupied White Bishop, Occupied White Queen, Occupied White King, Occupied White Bishop, Occupied White Knight, Occupied White Rook],
          Row [Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          Row [Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn],
          Row [Occupied Black Rook, Occupied Black Knight, Occupied Black Bishop, Occupied Black Queen, Occupied Black King, Occupied Black Bishop, Occupied Black Knight, Occupied Black Rook]
        ]
    )
    Black
    (Castling True True True True)
    Nothing
    0
    1

-- test_position3 :: Position
-- test_position3 = Position
--   (Board
--     [ Row [Occupied White Rook, Occupied White Knight, Occupied White Bishop, Occupied White Queen, Occupied White King, Occupied White Bishop, Occupied White Knight, Occupied White Rook]
