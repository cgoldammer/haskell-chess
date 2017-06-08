{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}

module Pgn (
    startGameFen
  , pgnToMove
  , PgnType (Standard, WithColumn, WithRow, WithBoth)
  , possibleMoveFields
  , pgnPiece
  , pgnToTargetField
  , moveToPgn) where

import Board
import Logic
import Helpers

import Control.Applicative
import Data.Aeson
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.Char as Ch
import Data.List
import Data.Maybe
import qualified Data.Text as Te

import Board
import Logic
import Various

type PgnMove = String

pgnMove :: GameState -> Move -> PgnMove
pgnMove = undefined

possibleMoveFields :: GameState -> PgnMove -> [(Move, [String])]
possibleMoveFields gs pgn = zip moves pgnMoves
    where   movePiece = pgnPiece pgn
            allMoves = allNextLegalMoves gs
            allPieceFields = getPositions gs movePiece
            targetField = pgnToTargetField pgn
            movesWithPieceFields = [(mv, pieceFieldForMove gs mv) | mv <- allMoves, targetField == Just (moveTo mv) && pfPiece (pieceFieldForMove gs mv) == movePiece && targetField == Just (moveTo mv)]
            moves = fmap fst movesWithPieceFields
            pgnMoves = createPgnMoves gs movesWithPieceFields

pgnToTargetField pgn = stringToField $ fmap Ch.toUpper $ reverse $ Data.List.take 2 $ reverse pgn

pgnToMove :: GameState -> PgnMove -> Maybe Move
pgnToMove gs pgn = listToMaybe [mv | (mv, pgnList) <- possibleMoveFields gs pgn, pgn `elem` pgnList]

pgnPiece :: PgnMove -> Piece
pgnPiece pgnMove
  | head pgnMove `elem` ("KQRNB" :: String) = fromJust $ stringToPiece $ [head pgnMove]
  | otherwise = Pawn



createPgnMoves :: GameState -> [(Move, PieceField)] -> [[PgnMove]]
createPgnMoves gs ls
    | length ls == 1 = [[moveToPgn Standard (fst (head ls)) (snd (head ls))]]
    | otherwise = fmap (\el -> expandMove gs (fst el) (snd el)) ls


expandMove :: GameState -> Move -> PieceField -> [PgnMove]
expandMove gs mv pf = [withColumn, withRow, withBoth]
    where   withColumn = moveToPgn WithColumn mv pf
            withRow = moveToPgn WithRow mv pf
            withBoth = moveToPgn WithBoth mv pf
        

data PgnType = Standard | WithColumn | WithRow | WithBoth

moveToPgn :: PgnType -> Move -> PieceField -> PgnMove
moveToPgn Standard mv pf = moveToPgnHelper False False mv pf
moveToPgn WithColumn mv pf = moveToPgnHelper True False mv pf
moveToPgn WithRow mv pf = moveToPgnHelper False True mv pf
moveToPgn WithBoth mv pf = moveToPgnHelper True True mv pf

moveToPgnHelper :: Bool -> Bool -> Move -> PieceField -> PgnMove
moveToPgnHelper withColumn withRow mv pf = concat $ catMaybes values
  where
    pieceString = pgnPieceChar $ pfPiece pf
    columnString = fmap Ch.toLower $ shortColumn $ fieldColumn $ moveFrom mv
    rowString = shortRow $ fieldRow $ moveFrom mv
    targetString = fmap Ch.toLower $ shortField $ moveTo mv
    values = [Just pieceString, makeMaybe withColumn columnString, makeMaybe withRow rowString, Just targetString]

pgnPieceChar :: Piece -> String
pgnPieceChar piece = filter (not . (`elem` ("P"::String))) $ shortPiece piece




startGameFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

-- Nf3
-- Find all moves possible by that piece
-- If it's simple form, e.g. Nf3, then find that move and make it
-- Special case:
-- N1f3
-- Ng1f3 Ng5f3
-- Patterns: Keep destination field. Remove either column or row from starting field
-- if there is a match, continue, otherwise try the other one
-- if none match at all, throw an error






pgnGame :: [pgnMove] -> Game
pgnGame = undefined
-- pgnGame pgnMoves = foldl move startGameState pgnMoves

data Game = Game { startingGameState :: GameState, gameMoves :: [Move] }


-- In other words, IO gives me [Game]. Then use it.



