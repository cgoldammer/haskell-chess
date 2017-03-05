module Lib where

import Data.Maybe
import Data.List


runChess :: IO ()
runChess = putStrLn "runChess"

data Piece = King | Queen | Rook | Bishop | Knight
data Column = A | B | C | D | E | F | G | H deriving (Enum, Ord, Eq, Show)
data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Enum, Ord, Eq, Show)

allColumns = [A ..]
allRows = [R1 ..]


data Color = White | Black


type Field = (Column, Row)
type Move = (Field, Field)
type PieceField = (Piece, Color, Field)
type Position = [PieceField]

step :: Position -> Move -> Maybe Position
step = undefined

validMoves :: Position -> [Move]
validMoves = undefined

playerPieces :: Position -> Color -> [PieceField]
playerPieces = undefined

potentialMoves :: PieceField -> [Move]
potentialMoves = undefined

moveIsLegal :: Position -> Move -> Bool
moveIsLegal = undefined

legalMoves :: Position -> PieceField -> [Move]
legalMoves ps pf = filter (moveIsLegal ps) (potentialMoves pf)

rookMoves :: Field -> [Field]
rookMoves sf@(c, r) = filter (/=sf) fields
    where fields = rowFields ++ columnFields
          rowFields = [(c, row) | row <- allRows]
          columnFields = [(column, r) | column <- allColumns]


columnInt :: Column -> Int
columnInt c = fromJust $ elemIndex c allColumns

rowInt :: Row -> Int
rowInt r = fromJust $ elemIndex r allRows

intRow = (flip index) allRows
intColumn = (flip index) allColumns

fieldStep :: Field -> (Int, Int) -> Maybe Field
fieldStep (c, r) (x, y) = maybeFromCondition (newCol, newRow) allLegit
    where   cInt = columnInt c
            rInt = rowInt r
            newX = (cInt - x)
            newY = (rInt - y)
            newCol = fromJust $ intColumn newX
            newRow = fromJust $ intRow newY
            allLegit = (isJust (intColumn newX)) && (isJust (intRow newY))

maybeFromCondition :: a -> Bool -> Maybe a
maybeFromCondition _ False = Nothing
maybeFromCondition a _ = Just a


knightSteps = [(sc, sr) | sc <- [-2, -1, 1, 2], sr <- [-2, -1, 1, 2], abs(sc) + abs(sr) == 3]

knightMoves :: Field -> [Field]
knightMoves sf = catMaybes $ fmap (fieldStep sf) knightSteps

foo :: Int -> (Int, Int)
foo _ = (1, 2)



index :: Int -> [a] -> Maybe a
index _ []       = Nothing
index 0 (x : _)  = Just x
index i (_ : xs) = index (i - 1) xs
