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

legitStartingPosition :: GameState -> Bool
legitStartingPosition gs = hasWhiteKing && hasBlackKing && noDuplicatedFields && notChecking
    where   notChecking = not $ isChecking gs
            whiteKingPositions = whitePiecePositions King gs
            blackKingPositions = blackPiecePositions King gs
            hasWhiteKing = (length whiteKingPositions) == 1
            hasBlackKing = (length blackKingPositions) == 1
            -- finds = M.fromList [(pc, (whitePiecePosition pc gs, blackPiecePosition pc gs)]
            -- queens = M.lookup 
            apf = fmap pfField (gsPosition gs) :: [Field]
            numberDistinctFields = length $ Un.count apf
            numberFields = length apf
            noDuplicatedFields = numberDistinctFields == numberFields
            
randomPiece :: (RandomGen g) => Rand g PieceField
randomPiece = do 
    pos <- getRandomR (0, (length allPieceFields) - 1)
    return $ allPieceFields !! pos
    
randomPieces :: (RandomGen g) => Int -> Rand g [PieceField]
randomPieces n = sequence (replicate n randomPiece)

nextRandomGameState :: IO GameState
nextRandomGameState = do
    position <- evalRandIO (randomPieces 14)
    let gs = GameState position White ((False, False), (False, False)) Nothing 0 1
    return gs


randomPositions :: Int -> IO [GameState]
randomPositions n = sequence (replicate n nextRandomGameState)

randomGood :: Int -> IO [GameState]
randomGood n = do
    pos <- randomPositions n
    let legit = filter legitStartingPosition pos
    return legit


-- matesFromGS :: [GameState] -> [(GameState, Int, [(Move, Move, Move)])]
-- matesFromGS gs = [(gs, length moves, moves) | (gs, moves) <- zip gs (fmap matesInTwo gs), length moves > 0]

-- forcedMatesFromStd = matesFromGS . randomPositions

mateInTwo = gsFromString ["WKA1", "BKH7", "WRB6", "WRC3"]

gsFromString s = GameState (fromJust (stringToPosition s)) White
allPieceFieldsWhite = [PieceField piece White (col, row) | piece <- [King] ++ allPieces, col <- allColumns, row <- allRows]
allPieceFieldsBlack = [PieceField piece Black (col, row) | piece <- [King] ++ allPieces, col <- allColumns, row <- allRows]
allPieceFields = allPieceFieldsWhite ++ allPieceFieldsBlack
allFields = [(col, row) | col <- allColumns, row <- allRows]

strings = [ ["WKA1", "WQA2", "WRA7", "WRB6", "BKH8"],
            ["WKA1", "WQA3", "WQA4", "WRA7", "WRB6", "BKH8"]]
            

