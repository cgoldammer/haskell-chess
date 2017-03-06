module Lib where

import Data.Maybe
import Data.List
import qualified Data.Set as S


runChess :: IO ()
runChess = putStrLn "runChess"

data Piece = King | Queen | Rook | Bishop | Knight deriving (Enum, Eq, Show)
data Column = A | B | C | D | E | F | G | H deriving (Enum, Ord, Eq, Show)
data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Enum, Ord, Eq, Show)

allColumns = [A ..]
allRows = [R1 ..]

data Color = White | Black deriving (Enum, Ord, Eq, Show)

type Field = (Column, Row)
type Move = (Field, Field)
data PieceField = PieceField {pfPiece :: Piece, pfColor :: Color, pfField :: Field} deriving (Eq, Show)
type Position = [PieceField]

inCheck :: GameState -> Bool
inCheck gs = kingPosition `elem` allOpponentMoves
    where allOpponentMoves = fmap snd $ allNextLegalMoves gs
          kingPosition = (getPositions gs King) !! 0

moveIsLegal :: GameState -> Move -> Bool
moveIsLegal gs mv@(from, to) = notInCheck  && notTakingOwn && notJumping
    where notInCheck = not $ inCheck gs
          notJumping = not $ isJumping piece position mv
          piece = pfPiece $ (filter (\pf -> pfField pf == from) position) !! 0
          potentialNewState = move gs mv
          notTakingOwn = not $ to `elem` allOwnPositions
          color = gsColor gs
          position = gsPosition gs
          allOwnPositions = fmap pfField $ filter (\pf -> (pfColor pf == color)) position

inter :: Ord a => [a] -> [a] -> S.Set a
inter l1 l2 = S.intersection (S.fromList l1) (S.fromList l2)

isJumpingPiece :: (Field -> [Field]) -> Position -> Move -> Bool
isJumpingPiece mf ps mv = length (inter inBetweenFields (fmap pfField ps)) > 0
    where   inBetweenFields = fmap fst $ filter (\m -> (moveDistance m) < distance) moves
            moves = zip (repeat from) (mf from)
            distance = moveDistance mv
            (from, to) = mv

moveDistance (from, to) = abs (fromX - toX) + abs (fromY - toY)
    where (fromX, fromY) = fieldToInt from
          (toX, toY) = fieldToInt to

fieldToInt :: Field -> (Int, Int)
fieldToInt (c, r) = (columnInt c, rowInt r)

isJumping :: Piece -> Position -> Move -> Bool
isJumping Rook = isJumpingPiece rookMoves
isJumping Bishop = isJumpingPiece bishopMoves
isJumping Queen = isJumpingPiece queenMoves
isJumping _ = \ps mv -> False


getPositions :: GameState -> Piece -> [Field]
getPositions gs pc = fmap pfField $ filter isRightPiece position
    where   position = gsPosition gs
            color = gsColor gs
            isRightPiece (PieceField pc color field) = True
            isRightPiece (PieceField _ _ _) = False
            
data GameState = GameState {gsPosition :: Position, gsColor :: Color} deriving (Eq, Show)

move :: GameState -> Move -> GameState
move gs (from, to) = GameState newPosition newColor
    where   newColor = invertColor oldColor
            oldColor = gsColor gs
            oldPosition = gsPosition gs
            removePosition = filter (\pf -> not (elem (pfField pf) [from, to])) oldPosition
            movedPiece = pfPiece $ (selectByPosition oldPosition [from] !! 0)
            newPosition = removePosition ++ [PieceField movedPiece oldColor to]

selectByPosition :: Position -> [Field] -> Position
selectByPosition ps fs = filter (\pf -> (elem (pfField pf) fs)) ps

invertColor White = Black
invertColor Black = White

allNextMoves :: GameState -> [Move]
allNextMoves gs = concat [zip (repeat (pfField pf)) (pieceMoves (pfPiece pf) (pfField pf)) | pf <- (gsPosition gs), gsColor gs == pfColor pf]

allNextLegalMoves :: GameState -> [Move]
allNextLegalMoves gs = filter (moveIsLegal gs) (allNextMoves gs)

allNextStates :: GameState -> [GameState]
allNextStates gs = fmap (move gs) (allNextLegalMoves gs)

isMate :: GameState -> Bool
isMate gs = noNextMoves && (not (inCheck gs))
    where noNextMoves = length (allNextMoves gs) == 0
    



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


movesFromSteps :: [(Int, Int)] -> Field -> [Field]
movesFromSteps steps sf = catMaybes $ fmap (fieldStep sf) steps

knightSteps = [(sc, sr) | sc <- [-2, -1, 1, 2], sr <- [-2, -1, 1, 2], abs(sc) + abs(sr) == 3]
knightMoves = movesFromSteps knightSteps

rookSteps = [(sc, sr) | sc <- [-7..7], sr <- [-7..7], abs(sc) + abs(sr) > 0, sc == 0 || sr == 0]
rookMoves = movesFromSteps rookSteps

bishopSteps = [(sc, sr) | sc <- [-7..7], sr <- [-7..7], abs(sc) + abs(sr) > 0, abs(sc) == abs(sr)]
bishopMoves = movesFromSteps bishopSteps

kingSteps = [(sc, sr) | sc <- [-1..1], sr <- [-1..1], abs(sc) + abs(sr) > 0]
kingMoves = movesFromSteps kingSteps

queenMoves sf = rookMoves sf ++ bishopMoves sf

pieceMoves Queen = queenMoves
pieceMoves Knight = knightMoves
pieceMoves Bishop = bishopMoves
pieceMoves Rook = rookMoves
pieceMoves King = kingMoves

foo :: Int -> (Int, Int)
foo _ = (1, 2)

-- A safe version of (!!) that returns Nothing if the position doesn't exist
index :: Int -> [a] -> Maybe a
index _ []       = Nothing
index 0 (x : _)  = Just x
index i (_ : xs) = index (i - 1) xs
