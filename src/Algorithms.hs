{-# LANGUAGE FlexibleInstances, OverlappingInstances, ScopedTypeVariables #-}

module Algorithms (randomGood, randomPositions) where

import Board
import Logic
import System.Random
import qualified Data.List.Unique as Un
import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Random

matesInOne :: GameState -> [Move]
matesInOne gs = fmap fst $ filter (\s -> isMate (snd s)) movePositions
    where   allMoves = allNextLegalMoves gs
            movePositions = zip allMoves (fmap (move gs) allMoves)

isMateInOne :: GameState -> Bool
isMateInOne gs = length (matesInOne gs) > 0

piecePositions :: Piece -> GameState -> Color -> [Field]
piecePositions pc gs White = getPositions gs pc
piecePositions pc gs Black = getPositions (invertGameStateColor gs) pc

legalPosition :: GameState -> Bool
legalPosition gs = bothSidesOneKing && noDuplicatedFields && not (isChecking gs)
    where   numberKings = fmap (length . piecePositions King gs) [White, Black]
            bothSidesOneKing = (maximum numberKings == 1) && (minimum numberKings == 1)
            noDuplicatedFields = (length . Un.repeated . fmap pfField . gsPosition) gs == 0

sensiblePosition :: GameState -> Bool
sensiblePosition gs = maxLightPieceCount == 2 && length queens == 1 && not sameColorBishops
  where     lightPieceCount = fmap (length . getPositions gs) lightPieces
            maxLightPieceCount = maximum lightPieceCount
            queens = getPositions gs Queen
            bishops = getPositions gs Bishop
            bishopColors = fmap fieldColor bishops
            sameColorBishops = Un.unique bishopColors /= bishopColors

goodPosition :: GameState -> Bool
goodPosition = liftM2 (&&) legalPosition sensiblePosition

lightPieces = [Rook, Bishop, Knight]

randomFromFields :: (RandomGen g) => Color -> Piece -> [Field] -> Rand g PieceField
randomFromFields color piece fields = do 
    pos <- getRandomR (0, (length fields) - 1)
    return $ PieceField piece color (fields !! pos)

randomFortress :: (RandomGen g) => Rand g Position
randomFortress = do 
    pos <- getRandomR (0, (length blackFortresses) - 1)
    return $ blackFortresses !! pos

randomPiece :: (RandomGen g) => Rand g PieceField
randomPiece = do 
    pos <- getRandomR (0, (length allPieceFields) - 1)
    return $ allPieceFields !! pos
    
randomPieces :: (RandomGen g) => Int -> Rand g [PieceField]
randomPieces n = sequence (replicate n randomPiece)

whiteKingFields = [Field col R1 | col <- allColumns]

nextRandomGameState :: IO GameState
nextRandomGameState = do
    basePosition <- evalRandIO $ randomPieces 8
    whiteKingPosition <- evalRandIO $ randomFromFields White King whiteKingFields
    blackFortress <- evalRandIO $ randomFortress
    let position = basePosition ++ [whiteKingPosition] ++ blackFortress
    let gs = GameState position White ((False, False), (False, False)) Nothing 0 1
    return gs

blackFortressStrings = [
      ["BKG8", "BPF7", "BPG7", "BPH7", "BRE8", "BQD8"]
    , ["BKH8", "BPF6", "BPG7", "BPH7", "BRD8", "BBC7"]
    , ["BKG8", "BPF7", "BPG6", "BPH6", "BPE7", "BBE8"]]

blackFortresses = catMaybes $ fmap stringToPosition blackFortressStrings

randomPositions :: Int -> IO [GameState]
randomPositions n = sequence (replicate n nextRandomGameState)

randomGood :: Int -> IO [GameState]
randomGood = fmap (filter goodPosition) . randomPositions

gsFromString s = GameState (fromJust (stringToPosition s)) White

pawnOnBadRow :: Piece -> Row -> Bool
pawnOnBadRow Pawn R1 = True
pawnOnBadRow Pawn R8 = True
pawnOnBadRow _ _ = False

piecesAndFields = [(piece, Field col row) | piece <- allNonKingPieces, col <- allColumns, row <- allRows, not (pawnOnBadRow piece row)]
turnIntoPieceField color = \(piece, Field col row) -> PieceField piece color (Field col row)

allPieceFields = (fmap (turnIntoPieceField White) piecesAndFields) ++ (fmap (turnIntoPieceField Black) piecesAndFields)
allFields = [Field col row | col <- allColumns, row <- allRows]



            


