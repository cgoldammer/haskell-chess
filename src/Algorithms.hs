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


-- A mate in two means: There exist a first move such that, for any opponents move
-- There is a mate in one.
matesInTwo :: GameState -> [(Move, Move, Move)]
matesInTwo gs = do
    firstMove :: Move <- allNextLegalMoves gs
    let newState = move gs firstMove
    guard $ opponentIsMatable newState
    guard $ not $ isMateInOne gs

    opponentMove <- allNextLegalMoves newState
    let afterOpponentMove = move newState opponentMove
    guard $ not $ isMate afterOpponentMove

    mateMove <- matesInOne afterOpponentMove

    return (firstMove, opponentMove, mateMove)


opponentMates :: GameState -> [(Move, Move)]
opponentMates gs = do
    firstMove :: Move <- allNextLegalMoves gs
    let newState = move gs firstMove
    mateMove <- matesInOne newState
    return (firstMove, mateMove)
    

opponentIsMatable :: GameState -> Bool
opponentIsMatable gs = (length (Un.count (fmap fst (opponentMates gs)))) == length (allNextLegalMoves gs)

whitePiecePositions :: Piece -> GameState -> [Field]
whitePiecePositions pc gs = getPositions gs pc

blackPiecePositions :: Piece -> GameState -> [Field]
blackPiecePositions pc gs = getPositions (invertGameStateColor gs) pc

legalPosition :: GameState -> Bool
legalPosition gs = bothSidesOneKing && noDuplicatedFields && notChecking
    where   whiteKingPositions = whitePiecePositions King gs
            blackKingPositions = blackPiecePositions King gs
            hasWhiteKing = (length whiteKingPositions) == 1
            hasBlackKing = (length blackKingPositions) == 1
            bothSidesOneKing = hasWhiteKing && hasBlackKing
            notChecking = not $ isChecking gs
            numberDistinctFields = length $ Un.count apf
            apf = fmap pfField (gsPosition gs) :: [Field]
            numberFields = length apf
            noDuplicatedFields = numberDistinctFields == numberFields

sensiblePosition :: GameState -> Bool
sensiblePosition gs = maxLightPieceCount == 2 && length queens == 1 && not duplicatedColors
  where     
            lightPieceCount = fmap (length . getPositions gs) lightPieces
            maxLightPieceCount = maximum lightPieceCount
            bishops = getPositions gs Bishop
            queens = getPositions gs Queen
            bishopColors = fmap fieldColor bishops
            duplicatedColors = Un.unique bishopColors /= bishopColors

goodPosition :: GameState -> Bool
goodPosition = liftM2 (&&) legalPosition sensiblePosition

lightPieces = [Rook, Bishop, Knight]


randomPiece :: (RandomGen g) => Rand g PieceField
randomPiece = do 
    pos <- getRandomR (0, (length allPieceFields) - 1)
    return $ allPieceFields !! pos
    
randomPieces :: (RandomGen g) => Int -> Rand g [PieceField]
randomPieces n = sequence (replicate n randomPiece)

nextRandomGameState :: IO GameState
nextRandomGameState = do
    position <- evalRandIO (randomPieces 8)
    let gs = GameState position White ((False, False), (False, False)) Nothing 0 1
    return gs

randomPositions :: Int -> IO [GameState]
randomPositions n = sequence (replicate n nextRandomGameState)

randomGood :: Int -> IO [GameState]
randomGood = fmap (filter goodPosition) . randomPositions

gsFromString s = GameState (fromJust (stringToPosition s)) White

pawnOnBadRow :: Piece -> Row -> Bool
pawnOnBadRow Pawn R1 = True
pawnOnBadRow Pawn R8 = True
pawnOnBadRow _ _ = False

piecesAndFields = [(piece, Field col row) | piece <- allPieces, col <- allColumns, row <- allRows, not (pawnOnBadRow piece row)]
turnIntoPieceField color = \(piece, Field col row) -> PieceField piece color (Field col row)
allPieceFields = (fmap (turnIntoPieceField White) piecesAndFields) ++ (fmap (turnIntoPieceField Black) piecesAndFields)
allFields = [Field col row | col <- allColumns, row <- allRows]



            


