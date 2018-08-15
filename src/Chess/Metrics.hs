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
, gsOppKings :: Bool
, gsPieceValues :: (Int, Int)
, gsQueens :: (Int, Int)
, gsRooks :: (Int, Int)
}


data StatType = NumberMovesOwn
              | NumberMovesOpp
              | NumberChecksOwn
              | NumberChecksOpp
              | NumberTakesOwn
              | NumberTakesOpp
              | OppKings 
              | PieceValuesOwn 
              | PieceValuesOpp 
              | QueensOwn
              | QueensOpp
              | RooksOwn
              | RooksOpp deriving (Eq, Enum)

type GameStateData = (StatType, Int)

gameStateData :: GameStateStats -> [GameStateData]
gameStateData gsd = [
    numMovesOwnDat, numMovesOppDat,
    numChecksOwnDat, numChecksOppDat, 
    numTakesOwnDat, numTakesOppDat,
    numChecksOppDat, oppKingsDat, 
    pieceValuesOwnDat, pieceValuesOppDat, 
    queensOwnDat, queensOppDat, 
    rooksOwnDat, rooksOppDat]
  where 
    numMovesOwnDat = (NumberMovesOwn, (fst . gsNumberMoves) gsd)
    numMovesOppDat = (NumberMovesOpp, (snd . gsNumberMoves) gsd)
    numChecksOwnDat = (NumberChecksOwn, (fst . gsNumberChecks) gsd)
    numChecksOppDat = (NumberChecksOpp, (snd . gsNumberChecks) gsd)
    numTakesOwnDat = (NumberTakesOwn, (fst . gsNumberTakes) gsd)
    numTakesOppDat = (NumberTakesOpp, (snd . gsNumberTakes) gsd)
    oppKingsDat = (OppKings, if gsOppKings gsd then 1 else 0)
    pieceValuesOwnDat = (PieceValuesOwn, (fst . gsPieceValues) gsd)
    pieceValuesOppDat = (PieceValuesOpp, (snd . gsPieceValues) gsd)
    queensOwnDat = (QueensOwn, (fst . gsQueens) gsd)
    queensOppDat = (QueensOpp, (snd . gsQueens) gsd)
    rooksOwnDat = (RooksOwn, (fst . gsPieceValues) gsd)
    rooksOppDat = (RooksOpp, (snd . gsPieceValues) gsd)

numberChecks :: [GameState] -> Int
numberChecks = length . (filter isChecking)

numberTakes :: GameState -> [(Piece, Move)] -> Int
numberTakes gs moves = length $ filter id $ fmap (isTaking gs) $ fmap (_moveTo . snd) moves

pieceVal :: Piece -> Int
pieceVal King = 0
pieceVal Queen = 9
pieceVal Rook = 5
pieceVal Bishop = 3
pieceVal Knight = 3
pieceVal Pawn = 1


bothColors :: (a -> b) -> a -> a -> (b, b)
bothColors metricFunction valOwn valOpp = (metricFunction valOwn, metricFunction valOpp)

pieceValues :: [PieceField] -> Int
pieceValues pf = sum $ fmap (pieceVal . _pfPiece) pf

numberPieces :: Piece -> [PieceField] -> Int
numberPieces pc pf = length $ filter ((==pc) . _pfPiece) pf

opposingKings :: [PieceField] -> [PieceField] -> Bool
opposingKings pfOwn pfOpp = maybe True (uncurry isSameSide) cols
  where
    getKing pieceFields = fmap (columnInt . _fieldColumn . _pfField) $ listToMaybe $ filter ((==King) . _pfPiece) pieceFields
    (wKingCol, bKingCol) = (getKing pfOwn, getKing pfOpp)
    cols = liftM2 (,)  wKingCol bKingCol

isSameSide :: Int -> Int -> Bool
isSameSide wCol bCol = (wCol <= 4 && bCol <= 4) || (wCol > 4 && bCol > 4)

getStats :: GameState -> GameStateStats
getStats gs = GameStateStats numMoves numChecks numTakes oppKings values queens rooks
  where
    gsOpp = invertGameStateColor gs
    movesOwn = allLegalMoves gs
    movesOpp = allLegalMoves gsOpp
    numMoves = (length movesOwn, length movesOpp)
    nextStatesOwn = fmap (uncurry (move gs)) movesOwn
    nextStatesOpp = fmap (uncurry (move gsOpp)) movesOpp
    numChecks = (numberChecks nextStatesOwn, numberChecks nextStatesOpp)
    numTakes = (numberTakes gs movesOwn, numberTakes gsOpp movesOpp)
    pieceFields = _gsPosition gs
    color = _gsColor gs
    pfOwn = filter ((==color) . _pfColor) pieceFields
    pfOpp = filter ((/=color) . _pfColor) pieceFields
    values = bothColors pieceValues pfOwn pfOpp
    queens = bothColors (numberPieces Queen) pfOwn pfOpp
    rooks = bothColors (numberPieces Rook) pfOwn pfOpp
    oppKings = opposingKings pfOwn pfOpp

