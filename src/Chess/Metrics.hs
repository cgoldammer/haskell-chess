-- Metrics on a given `GameState`.
-- This module is experimental. I use it in a current project to determine
-- which games and positions are sharper than others. 
module Chess.Metrics (gameStateData, getStats, StatType(..)) where

import Data.Maybe (listToMaybe)
import Control.Monad (liftM2)

import Chess.Types
import Chess.Logic

data GameStateStats = GameStateStats {
  gsNumberMoves :: (Int, Int)
, gsNumberChecks :: (Int, Int)
, gsNumberTakes :: (Int, Int)
, gsNumberTakesPawn :: (Int, Int)
, gsOppKings :: Bool
, gsPieceValues :: (Int, Int)
, gsQueens :: (Int, Int)
, gsRooks :: (Int, Int)
, gsKingPawns :: (Int, Int)
, gsKingRow :: (Int, Int)
, isInCheck :: Bool
} deriving (Show)


data StatType = NumberMovesOwn
              | NumberMovesOpp
              | NumberChecksOwn
              | NumberChecksOpp
              | NumberTakesOwn
              | NumberTakesOpp
              | NumberTakesPawnOwn
              | NumberTakesPawnOpp
              | OppKings 
              | PieceValuesOwn 
              | PieceValuesOpp 
              | KingPawnsOwn
              | KingPawnsOpp
              | KingRowOwn
              | KingRowOpp
              | QueensOwn
              | QueensOpp
              | RooksOwn
              | RooksOpp
              | IsInCheck deriving (Eq, Enum)

type GameStateData = (StatType, Int)

gameStateData :: GameStateStats -> [GameStateData]
gameStateData gsd = [
    numMovesOwnDat, numMovesOppDat,
    numChecksOwnDat, numChecksOppDat, 
    numTakesOwnDat, numTakesOppDat,
    numTakesPawnOwnDat, numTakesPawnOppDat,
    numChecksOppDat, oppKingsDat, 
    pieceValuesOwnDat, pieceValuesOppDat, 
    queensOwnDat, queensOppDat, 
    rooksOwnDat, rooksOppDat,
    kingPawnsOwnDat, kingPawnsOppDat,
    kingRowOwnDat, kingRowOppDat,
    isInCheckDat]
  where 
    numMovesOwnDat = (NumberMovesOwn, (fst . gsNumberMoves) gsd)
    numMovesOppDat = (NumberMovesOpp, (snd . gsNumberMoves) gsd)
    numChecksOwnDat = (NumberChecksOwn, (fst . gsNumberChecks) gsd)
    numChecksOppDat = (NumberChecksOpp, (snd . gsNumberChecks) gsd)
    numTakesOwnDat = (NumberTakesOwn, (fst . gsNumberTakes) gsd)
    numTakesOppDat = (NumberTakesOpp, (snd . gsNumberTakes) gsd)
    numTakesPawnOwnDat = (NumberTakesPawnOwn, (fst . gsNumberTakesPawn) gsd)
    numTakesPawnOppDat = (NumberTakesPawnOpp, (snd . gsNumberTakesPawn) gsd)
    oppKingsDat = (OppKings, if gsOppKings gsd then 1 else 0)
    pieceValuesOwnDat = (PieceValuesOwn, (fst . gsPieceValues) gsd)
    pieceValuesOppDat = (PieceValuesOpp, (snd . gsPieceValues) gsd)
    queensOwnDat = (QueensOwn, (fst . gsQueens) gsd)
    queensOppDat = (QueensOpp, (snd . gsQueens) gsd)
    rooksOwnDat = (RooksOwn, (fst . gsRooks) gsd)
    rooksOppDat = (RooksOpp, (snd . gsRooks) gsd)
    kingPawnsOwnDat = (KingPawnsOwn, (fst . gsKingPawns) gsd)
    kingPawnsOppDat = (KingPawnsOpp, (snd . gsKingPawns) gsd)
    kingRowOwnDat = (KingRowOwn, (fst . gsKingRow) gsd)
    kingRowOppDat = (KingRowOpp, (snd . gsKingRow) gsd)
    isInCheckDat = (IsInCheck, if isInCheck gsd then 1 else 0)

numberChecks :: [GameState] -> Int
numberChecks states = length . (filter isChecking) $ fmap invertGameStateColor states

numberTakes :: GameState -> [(Piece, Move)] -> Int
numberTakes gs moves = length $ filter id $ fmap (isTaking gs) $ fmap (_moveTo . snd) moves

numberTakesPawn :: GameState -> [(Piece, Move)] -> Int
numberTakesPawn gs moves = length $ filter id $ fmap (isTaking gs) $ fmap (_moveTo . snd) pawnMoves
  where pawnMoves = filter ((==Pawn) . fst) moves

pieceVal :: Piece -> Int
pieceVal King = 0
pieceVal Queen = 9
pieceVal Rook = 5
pieceVal Bishop = 3
pieceVal Knight = 3
pieceVal Pawn = 1


kingPawns :: [PieceField] -> Int
kingPawns pieceFields = length pawnsNearKing
  where pawnsNearKing = filter closeToKing pawnFields
        closeToKing field = maybe False (\kf -> moveDistance (field, kf) <= maxDistance) kingField
        maxDistance = 2
        kingField = getKing pieceFields
        pawnFields = fmap _pfField $ filter ((==Pawn) . _pfPiece) pieceFields

rowNumberForColor :: Color -> Row -> Int
rowNumberForColor White row = rowInt row
rowNumberForColor Black row = 9 - rowInt row

kingRow :: Color -> [PieceField] -> Int
kingRow color pieceFields = maybe 1 (\row -> rowNumberForColor color row) maybeRow
  where kingField = getKing pieceFields
        maybeRow = fmap _fieldRow kingField

bothColors :: (a -> b) -> a -> a -> (b, b)
bothColors metricFunction valOwn valOpp = (metricFunction valOwn, metricFunction valOpp)

pieceValues :: [PieceField] -> Int
pieceValues pf = sum $ fmap (pieceVal . _pfPiece) pf

numberPieces :: Piece -> [PieceField] -> Int
numberPieces pc pf = length $ filter ((==pc) . _pfPiece) pf

getKing :: Position -> Maybe Field
getKing pieceFields = fmap _pfField $ listToMaybe $ filter ((==King) . _pfPiece) pieceFields

getKingCol :: Position -> Maybe Int
getKingCol = fmap (columnInt . _fieldColumn) . getKing

opposingKings :: [PieceField] -> [PieceField] -> Bool
opposingKings pfOwn pfOpp = not $ maybe False (uncurry isSameSide) cols
  where
    (wKingCol, bKingCol) = (getKingCol pfOwn, getKingCol pfOpp)
    cols = liftM2 (,)  wKingCol bKingCol

isSameSide :: Int -> Int -> Bool
isSameSide wCol bCol = (wCol <= 4 && bCol <= 4) || (wCol > 4 && bCol > 4)

getStats :: GameState -> GameStateStats
getStats gs = GameStateStats numMoves numChecks numTakes numTakesPawn oppKings values queens rooks kp kingRows isInCheck
  where
    gsOpp = invertGameStateColor gs
    isInCheck = inCheck gs
    movesOwn = allLegalMoves gs
    movesOpp = if isInCheck then [] else allLegalMoves gsOpp
    numMoves = (length movesOwn, length movesOpp)
    nextStatesOwn = fmap (uncurry (move gs)) movesOwn
    nextStatesOpp = fmap (uncurry (move gsOpp)) movesOpp
    numChecks = (numberChecks nextStatesOwn, numberChecks nextStatesOpp)
    numTakes = (numberTakes gs movesOwn, numberTakes gsOpp movesOpp)
    numTakesPawn = (numberTakesPawn gs movesOwn, numberTakesPawn gsOpp movesOpp)
    pieceFields = _gsPosition gs
    color = _gsColor gs
    oppColor = invertColor color
    pfOwn = filter ((==color) . _pfColor) pieceFields
    pfOpp = filter ((/=color) . _pfColor) pieceFields
    values = bothColors pieceValues pfOwn pfOpp
    queens = bothColors (numberPieces Queen) pfOwn pfOpp
    rooks = bothColors (numberPieces Rook) pfOwn pfOpp
    oppKings = opposingKings pfOwn pfOpp
    kp = bothColors kingPawns pfOwn pfOpp :: (Int, Int)
    kingRows = (kingRow color pfOwn, kingRow oppColor pfOpp)

