{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Lib where

import Data.Maybe
import Data.List
import qualified Data.Set as S
import Control.Monad
import qualified Control.Lens as L
import qualified Data.Tuple as T
import Text.Read


runChess :: IO ()
runChess = putStrLn "runChess"

data Piece = King | Queen | Rook | Bishop | Knight deriving (Enum, Eq)
data Column = A | B | C | D | E | F | G | H deriving (Enum, Ord, Eq)
data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Enum, Ord, Eq)

allColumns = [A ..]
allRows = [R1 ..]

data Color = White | Black deriving (Enum, Ord, Eq)

type Field = (Column, Row)
type Move = (Field, Field)
data PieceField = PieceField {pfPiece :: Piece, pfColor :: Color, pfField :: Field} deriving (Eq)
type Position = [PieceField]


shortColor :: Color -> String
shortColor White = "W"
shortColor Black = "B"

shortPiece :: Piece -> String
shortPiece Knight = "N"
shortPiece King = "K"
shortPiece Queen = "Q"
shortPiece Rook = "R"
shortPiece Bishop = "B"

shortColumn :: Column -> String
shortColumn c = [['A'..'H'] !! (columnInt c)]

shortRow :: Row -> String
shortRow r = show $ rowInt r

shortField :: Field -> String
shortField (c, r) = shortColumn c ++ shortRow r

instance Show Color where
    show = shortColor

instance Show Piece where
    show = shortPiece

instance Show Row where
    show = shortRow

instance Show Column where
    show = shortColumn

instance Show Field where
    show (c, r) = show c ++ show r

instance Show PieceField where
    show (PieceField p c f) = show c ++ show p ++ show f

charColumn :: Char -> Maybe Column
charColumn c = join $ fmap (flip index allColumns) (elemIndex c ['A'..'H'])

charRow :: Char -> Maybe Row
charRow r = join $ fmap (flip index allRows) (elemIndex r ['1'..'8'])


stringToPiece :: String -> Maybe Piece
stringToPiece "K" = Just King
stringToPiece "R" = Just Rook
stringToPiece "B" = Just Bishop
stringToPiece "Q" = Just Queen
stringToPiece "N" = Just Knight
stringToPiece _ = Nothing

colorString :: String -> Maybe Color
colorString "W" = Just White
colorString "B" = Just Black
colorString _ = Nothing

sequence2Tuple = uncurry $ liftM2 (,)

stringToField :: String -> Maybe Field
stringToField [c, r] = sequence2Tuple (charColumn c, join (fmap intRow (readMaybe [r])))

stringToPieceField :: String -> Maybe PieceField 
stringToPieceField [colS, pieceS, cS, rS]
    | allPresent = Just $ PieceField (fromJust piece) (fromJust col) ((fromJust c), (fromJust r))
    | otherwise = Nothing
    where   col = colorString [colS]
            piece = stringToPiece [pieceS]
            c = charColumn cS
            r = charRow rS
            allPresent = (isJust col) && (isJust piece) && (isJust c) && (isJust r)

stringToPosition :: [String] -> Maybe Position
stringToPosition = traverse stringToPieceField


inter :: Ord a => [a] -> [a] -> S.Set a
inter l1 l2 = S.intersection (S.fromList l1) (S.fromList l2)

-- isJumpingPiece :: (Field -> [Field]) -> Position -> Move -> Bool
-- isJumpingPiece mf ps mv = length (inter inBetweenFields pieceFields) > 0
--     where   inBetweenFields = fmap fst inBetweenMoves
--             pieceFields = fmap pfField ps
--             inBetweenMoves = fmap snd $ filter (\(m, tg) -> (moveDistance tg) < distance) movesWithTogo
--             moves = zip (repeat from) (mf from)
--             toGo = zip (repeat to) (fmap snd moves)
--             movesWithTogo = zip moves toGo
--             distance = moveDistance mv
--             (from, to) = mv
-- isJumping :: Piece -> Position -> Move -> Bool
-- isJumping Rook = isJumpingPiece rookMoves
-- isJumping Bishop = isJumpingPiece bishopMoves
-- isJumping Queen = isJumpingPiece queenMoves
-- isJumping _ = \ps mv -> False


moveDistance (from, to) = abs (fromX - toX) + abs (fromY - toY)
    where (fromX, fromY) = fieldToInt from
          (toX, toY) = fieldToInt to

fieldToInt :: Field -> (Int, Int)
fieldToInt (c, r) = (columnInt c, rowInt r)


getPositions :: GameState -> Piece -> [Field]
getPositions gs@(GameState position color) pc = fmap pfField $ filter isRightPiece position
    where   isRightPiece (PieceField pfP pfC field) = (pfP == pc) && (pfC == color)
            
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

allNextLegalMoves :: GameState -> [Move]
allNextLegalMoves gs = filter notInCheck (allPhysicalMoves gs)
    where notInCheck mv = not (isChecking (move gs mv))

allNextStates :: GameState -> [GameState]
allNextStates gs = fmap (move gs) (allNextLegalMoves gs)

isMate :: GameState -> Bool
isMate gs = noNextMoves && (inCheck gs)
    where noNextMoves = length (allNextLegalMoves gs) == 0

allOpponentMoves :: GameState -> [Move]
allOpponentMoves = allPhysicalMoves . invertGameStateColor

invertGameStateColor :: GameState -> GameState
invertGameStateColor gs = GameState (gsPosition gs) (invertColor (gsColor gs))
    
inCheck :: GameState -> Bool
inCheck gs = kingPosition `elem` opponentMoves
    where opponentMoves = fmap snd $ allOpponentMoves gs
          kingPosition = (getPositions gs King) !! 0

isChecking :: GameState -> Bool
isChecking (GameState ps col) = inCheck (GameState ps (invertColor col))

type MoveDirections = [Move]

allPiecePhysicalMoves :: GameState -> PieceField -> [Move]
allPiecePhysicalMoves gs pf = goodMoves
    where fields = (pieceFields piece field) :: [[Field]]
          goodFields = (concat $ fmap (takeWhile isGood) fields) :: [Field]
          goodMoves = zip (repeat field) goodFields
          isGood f = not $ f `elem` (fmap pfField (ownPieceFields gs))
          piece = pfPiece pf
          field = pfField pf
          position = gsPosition gs
          color = gsColor gs

ownPieceFields :: GameState -> [PieceField]
ownPieceFields gs = filter (\pf -> (pfColor pf) == (gsColor gs)) (gsPosition gs)

allPhysicalMoves :: GameState -> [Move]
allPhysicalMoves gs = concat $ fmap (allPiecePhysicalMoves gs) (ownPieceFields gs)

-- allNextPhysicalMovesList :: GameState -> [[Move]]
-- allNextPhysicalMovesList gs = do 
--     pf <- gsPosition gs
--     let targetFields = pieceFields (pfPiece pf) (pfField pf)
--     let currentField = pfField pf
--     guard (gsColor gs == pfColor pf)
--     let moves = zip (repeat currentField) targetFields
--     return moves

columnInt :: Column -> Int
columnInt c = fromJust $ elemIndex c allColumns

rowInt :: Row -> Int
rowInt r = (fromJust (elemIndex r allRows)) + 1

intRow :: Int -> Maybe Row
intRow i = index (i - 1) allRows

intColumn :: Int -> Maybe Column
intColumn = (flip index) allColumns

fieldStep :: Field -> (Int, Int) -> Maybe Field
fieldStep (c, r) (x, y) = maybeFromCondition (newCol, newRow) allLegit
    where   cInt = columnInt c
            rInt = rowInt r
            newX = (cInt + x)
            newY = (rInt + y)
            newCol = fromJust $ intColumn newX
            newRow = fromJust $ intRow newY
            allLegit = (isJust (intColumn newX)) && (isJust (intRow newY))

maybeFromCondition :: a -> Bool -> Maybe a
maybeFromCondition _ False = Nothing
maybeFromCondition a _ = Just a

range :: Bool -> [Int]
range True = [1..7]
range False = reverse [(-7)..(-1)]

rangeT = range True
rangeF = range False

rookSteps = [zip rangeF (repeat 0), zip rangeT (repeat 0), zip (repeat 0) rangeF, zip (repeat 0) rangeT]
knightSteps = fmap (:[]) $ ([(sc, sr) | sc <- [-2, -1, 1, 2], sr <- [-2, -1, 1, 2], abs(sc) + abs(sr) == 3])
bishopSteps = [zip rangeF rangeT, zip rangeT rangeT, zip rangeF rangeF, zip rangeT rangeF]
queenSteps = rookSteps ++ bishopSteps
kingSteps = fmap (:[]) $ [(sc, sr) | sc <- [-1..1], sr <- [-1..1], abs(sc) + abs(sr) > 0]

movesFromSteps :: [[(Int, Int)]] -> Field -> [[Field]]
movesFromSteps steps sf = (fmap catMaybes) $ (fmap . fmap) (fieldStep sf) steps

pieceFields :: Piece -> Field -> [[Field]]
pieceFields Queen = movesFromSteps queenSteps
pieceFields Knight = movesFromSteps knightSteps
pieceFields Bishop = movesFromSteps bishopSteps
pieceFields Rook = movesFromSteps rookSteps
pieceFields King = movesFromSteps kingSteps

allPieceFields :: Piece -> Field -> [Field]
allPieceFields pc f = concat $ pieceFields pc f

-- A safe version of (!!) that returns Nothing if the position doesn't exist
index :: Int -> [a] -> Maybe a
index _ []       = Nothing
index 0 (x : _)  = Just x
index i (_ : xs) = index (i - 1) xs

testPS = fromJust $ stringToPosition ["WKA1", "BRA8", "BRB8", "BKC8"]
testGS = GameState testPS White
testGS2 = GameState testPS Black


