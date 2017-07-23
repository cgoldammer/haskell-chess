{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
-- Pawns, promotion, castling


module Logic (allPhysicalMoves
            , GameState (GameState, gsPosition, gsColor)
            , defaultGameState
            , getPositions
            , invertGameStateColor
            , allNextLegalMoves
            , move
            , isMate
            , basicFen
            , fullFen
            , isChecking
            , inCheck
            , pieceFields
            , pieceFieldForMove
            , parseFen
            , castlingRightsParser
            , allOpponentMoves
            , fenStringToPosition
            ) where

import Data.Maybe
import Board
import Helpers
import Control.Monad
import Control.Applicative
import qualified Data.String.Utils as SU
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Text.Read as TR
import Data.Attoparsec.Text hiding (take, D, takeWhile)
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.ByteString.Char8 as C
import Helpers
import Data.List
-- Todo
-- Pawns (1 vs 2 moves, capturing diagonally, en passant, promotion)
-- Castling


type CastlingRights = ((Bool, Bool), (Bool, Bool))

data GameState = GameState {
      gsPosition :: Position
    , gsColor :: Color
    , castlingRights :: CastlingRights
    , enPassantTarget :: Maybe Field
    , halfMove :: Int
    , fullMove :: Int} deriving (Eq, Show)

defaultGameState :: Position -> Color -> GameState
defaultGameState ps color = GameState ps color ((False, False), (False, False)) Nothing 0 1

movePiece :: GameState -> Move -> Piece
movePiece gs (Move from to _) = pfPiece $ (selectByPosition (gsPosition gs) [from] !! 0)

maybeMovePiece :: GameState -> Move -> Maybe Piece
maybeMovePiece gs (Move from to _) = fmap pfPiece $ index 0 (selectByPosition (gsPosition gs) [from])

move :: GameState -> (Piece, Move) -> GameState
move gs (piece, mv@(Move from to promotion)) = GameState newPosition newColor newCr newEp hm fm
    where   newColor = invertColor oldColor
            oldColor = gsColor gs
            oldPosition = gsPosition gs
            removePosition = filter (\pf -> not (elem (pfField pf) [from, to])) oldPosition
            newPosition = removePosition ++ [PieceField piece oldColor to]
            cr = castlingRights gs
            ept = enPassantTarget gs
            hm = halfMove gs
            fm = fullMove gs
            newCr = updateCastlingRights gs mv
            newEp = updateEnPassant gs mv

updateEnPassant :: GameState -> Move -> Maybe Field
updateEnPassant gs mv@(Move from to _)
    | pawnMovedTwo = beforeField 
    | otherwise = Nothing
    where
        pawnMovedTwo = movedPawn && distance == 2
        movedPawn = maybeMovePiece gs mv == Just Pawn
        distance = moveDistance (from, to)
        (Field fromC fromR) = from
        beforeRow = nextRow fromR (gsColor gs)
        beforeField = liftM2 Field (Just fromC) beforeRow

nextRow :: Row -> Color -> Maybe Row
nextRow r White = intRow $ (rowInt r) + 1
nextRow r Black = intRow $ (rowInt r) - 1

updateCastlingRights :: GameState -> Move -> CastlingRights
updateCastlingRights gs mv = updatedBothRights where
    updatedRights = (castleK && (not castleKLost), castleQ && (not castleQLost))
    castleWhite = fst cr
    castleBlack = snd cr
    cr = castlingRights gs
    (castleK, castleQ) = rights
    rights 
        | color == White = castleWhite
        | color == Black = castleBlack
    castleKLost = kingMoved || kingRookMoved
    castleQLost = kingMoved || queenRookMoved
    updatedBothRights
        | color == White = (updatedRights, castleBlack)
        | color == Black = (castleWhite, updatedRights)
    kingMoved = movePiece gs mv == King
    color = gsColor gs
    queenRookMoved = moveFrom mv == Field A R1
    kingRookMoved = moveFrom mv == Field H R1

allNextLegalMoves :: GameState -> [(Piece, Move)]
allNextLegalMoves gs = filter notInCheck $ allPhysicalMoves gs
    where notInCheck (piece, mv) = not (isChecking (move gs (piece, mv)))

pieceFieldForMove :: GameState -> Move -> PieceField
pieceFieldForMove gs mv = head [pf | pf <- gsPosition gs, moveFrom mv == pfField pf]

allNextStates :: GameState -> [GameState]
allNextStates gs = fmap (move gs) (allNextLegalMoves gs)

isMate :: GameState -> Bool
isMate gs = noNextMoves && (inCheck gs)
    where noNextMoves = length (allNextLegalMoves gs) == 0

allOpponentMoves :: GameState -> [(Piece, Move)]
allOpponentMoves = allPhysicalMoves . invertGameStateColor

invertGameStateColor :: GameState -> GameState
invertGameStateColor (GameState position color cr ept hm fm) = GameState position (invertColor color) cr ept hm fm
    
inCheck :: GameState -> Bool
inCheck gs = kingPosition `elem` (fmap Just opponentMoves)
    where opponentMoves = fmap (moveTo . snd) $ allOpponentMoves gs
          kingPosition = listToMaybe $ getPositions gs King

isChecking :: GameState -> Bool
isChecking = inCheck . invertGameStateColor
            

type MoveDirections = [Move]

allPiecePhysicalMoves :: GameState -> PieceField -> [(Piece, Move)]
allPiecePhysicalMoves gs@(GameState position color _ _ _ _) pf@(PieceField piece _ field) = zip (repeat piece) (goodMoves ++ castMoves)
    where fields = pieceFields pf
          castMoves = fmap nonPawnMove $ castlingMoves gs
          goodFields = concat $ fmap (takeWhile isGood) fields
          goodMoveFields = [(from, to) | (from, to) <- zip (repeat field) goodFields] :: [MoveLocation]
          goodMoveFilterPawn = filterPawnMoves gs piece goodMoveFields
          goodMoves = concat $ fmap (addSpecialMoves piece color) goodMoveFilterPawn
          isGood f = not $ f `elem` (fmap pfField (ownPieceFields gs))


filterPawnMoves :: GameState -> Piece -> [MoveLocation] -> [MoveLocation]
filterPawnMoves gs Pawn ml = filter (isLegalPawnMove gs) ml
filterPawnMoves _ _ ml = ml

nonPawnMove :: MoveLocation -> Move
nonPawnMove (from, to) = Move from to Nothing

isLegalPawnMove :: GameState -> MoveLocation -> Bool
isLegalPawnMove gs ml@(from, to) = (isGoingForward && notTaking) || (isGoingSideways && (taking || isEP))
    where   isGoingForward = (fieldColumn from == fieldColumn to) 
            isGoingSideways = (fieldColumn from /= fieldColumn to)
            taking = isTaking gs to
            notTaking = not taking
            isEP = isEnPassant gs ml

isTaking :: GameState -> Field -> Bool
isTaking gs to = to `elem` (fmap pfField (opponentPieceFields gs))

freeForCastling gs = [canCastleKingSide gs, canCastleQueenSide gs]

castlingMoves gs
    | gsColor gs == White = [(Field E R1, Field G R1), (Field E R1, Field E R1)]
    | gsColor gs == Black = [(Field E R8, Field G R8), (Field E R8, Field E R8)]
    
possibleCastlingMoves :: GameState -> [MoveLocation]
possibleCastlingMoves gs = catMaybes [useValueIfCondition m c | (m, c) <- zip (castlingMoves gs) (freeForCastling gs)]

useValueIfCondition :: a -> Bool -> Maybe a
useValueIfCondition a False = Nothing
useValueIfCondition a True = Just a

notInCheck = not . inCheck

canCastleKingSide :: GameState -> Bool
canCastleKingSide (GameState pos White ((False, _), _)  _ _ _) = False
canCastleKingSide (GameState pos Black ((_, False), _)  _ _ _) = False
canCastleKingSide gs@(GameState pos White ((True, _), _)  _ _ _) = (notInCheck gs) && (castlingFree gs [(Field F R1), (Field G R1)])
canCastleKingSide gs@(GameState pos Black ((_, True), _)  _ _ _) = (notInCheck gs) && (castlingFree gs [(Field F R8), (Field G R8)])

canCastleQueenSide :: GameState -> Bool
canCastleQueenSide gs@(GameState pos White (_, (False, _))  _ _ _) = False
canCastleQueenSide gs@(GameState pos Black (_, (_, False))  _ _ _) = False
canCastleQueenSide gs@(GameState pos White (_, (True, _))  _ _ _) = (notInCheck gs) && (castlingFree gs [(Field D R1), (Field C R1), (Field B R1)])
canCastleQueenSide gs@(GameState pos Black (_, (_, True))  _ _ _) = (notInCheck gs) && (castlingFree gs [(Field D R8), (Field C R8), (Field B R8)])

notCheckedOn :: GameState -> Field -> Bool
notCheckedOn = undefined

freeField :: GameState -> Field -> Bool
freeField = undefined

castlingFree :: GameState -> [Field] -> Bool
castlingFree gs fs = and $ fmap (\f -> (notCheckedOn gs f) && (freeField gs f)) fs

-- Logic for the `move` function:
-- In the EP case, remove the pawn that's EP taken
isEnPassant :: GameState -> MoveLocation -> Bool
isEnPassant gs ml@(from, to) = enPassantTarget gs == Just to


-- Update the castling permissions
-- In the castles case, move the rook as well as the King




addSpecialMoves :: Piece -> Color -> MoveLocation -> [Move]
addSpecialMoves Pawn White ml@((Field _ R7), _) = addPromotionMoves ml
addSpecialMoves Pawn Black ml@((Field _ R2), _) = addPromotionMoves ml
addSpecialMoves _ _ ml@((from, to)) = [Move from to Nothing]

addPromotionMoves :: MoveLocation -> [Move]
addPromotionMoves (from, to) = [Move from to (Just piece) | piece <- allNonKingFullPieces]

specialMoves :: GameState -> PieceField
specialMoves = undefined

ownPieceFields :: GameState -> [PieceField]
ownPieceFields gs = filter (\pf -> (pfColor pf) == (gsColor gs)) (gsPosition gs)

opponentPieceFields :: GameState -> [PieceField]
opponentPieceFields = ownPieceFields . invertGameStateColor 

allPhysicalMoves :: GameState -> [(Piece, Move)]
allPhysicalMoves gs = concat $ fmap (allPiecePhysicalMoves gs) (ownPieceFields gs)

fieldStep :: Field -> (Int, Int) -> Maybe Field
fieldStep (Field c r) (x, y) = maybeFromCondition (Field newCol newRow) allLegit
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

type StepMove = (Int, Int)
movesFromSteps :: [[StepMove]] -> Field -> [[Field]]
movesFromSteps steps sf = (fmap catMaybes) $ (fmap . fmap) (fieldStep sf) steps

pieceFields :: PieceField -> [[Field]]
pieceFields (PieceField King _ field) = movesFromSteps kingSteps field
pieceFields (PieceField Queen _ field) = movesFromSteps queenSteps field
pieceFields (PieceField Rook _ field) = movesFromSteps rookSteps field
pieceFields (PieceField Bishop _ field) = movesFromSteps bishopSteps field
pieceFields (PieceField Knight _ field) = movesFromSteps knightSteps field
pieceFields (PieceField Pawn color field) = movesFromSteps (pawnSteps field color) field

pawnSteps :: Field -> Color -> [[StepMove]]
pawnSteps (Field _ R1) _ = []
pawnSteps (Field _ R8) _ = []
pawnSteps (Field _ R2) White = [[(-1, 1)], [(0, 1), (0, 2)], [(1, -1)]]
pawnSteps (Field _ _) White = [[(-1, 1)], [(0, 1)], [(1,1)]]
pawnSteps (Field _ R7) Black = [[(-1, -1)], [(0, -1), (0, -2)], [(1,- 1)]]
pawnSteps _ Black = [[(-1, -1)], [(0, -1)], [(1, -1)]]

allPieceFields :: PieceField -> [Field]
allPieceFields = concat . pieceFields

getPositions :: GameState -> Piece -> [Field]
getPositions gs pc = fmap pfField $ filter isRightPiece position
    where   isRightPiece (PieceField pfP pfC field) = (pfP == pc) && (pfC == color)
            color = gsColor gs
            position = gsPosition gs
            

selectByPosition :: Position -> [Field] -> Position
selectByPosition ps fs = filter (\pf -> (elem (pfField pf) fs)) ps


type Fen = String
-- We want to prevent users from constructing Fen's without validating
-- them. The newtype is not exported, thus obtaining a ValidatedFen
-- requires using the `constructFen` function
newtype ValidatedFen = ValidatedFen Fen

constructFen :: String -> Maybe ValidatedFen
constructFen = undefined

pieceFieldFen :: Maybe PieceField -> Char
pieceFieldFen Nothing = '1'
pieceFieldFen (Just (PieceField piece color field)) = pieceChar
    where   pieceChar = transformer pieceLetter
            pieceLetter = (shortPiece piece) !! 0
            transformer 
                | color == White = id
                | color == Black = C.toLower

piecesOnField :: Position -> Field -> Maybe PieceField
piecesOnField ps f = index 0 (filter byField ps)
    where   byField pf = pfField pf == f

basicFenFromRow :: [PieceField] -> String
basicFenFromRow ps = fmap (\c -> pieceFieldFen (firstPieceOnColumn ps c)) allColumns

aggregateFen :: Int -> String -> String
aggregateFen i = SU.replace (take i (repeat '1')) (show i)

piecesOnRow :: Position -> Row -> Position
piecesOnRow ps r = filter (\pf -> fieldRow (pfField pf) == r) ps

firstPieceOnColumn :: Position -> Column -> Maybe PieceField
firstPieceOnColumn ps c = index 0 (filter (\pf -> fieldColumn (pfField pf) == c) ps)

positionByRow :: Position -> [Position]
positionByRow ps = fmap (piecesOnRow ps) (reverse allRows)

basicFen :: Position -> Fen
basicFen ps = intercalate "/" $ fmap (aggregator . basicFenFromRow) $ positionByRow ps

aggregator s = foldl (flip ($)) s (fmap aggregateFen (reverse [1..8]))


fullFen :: Position -> Fen
fullFen ps = "fen " ++ basicFen ps ++ " w - 0 1"

fenStringToPosition :: String -> Position
fenStringToPosition s = catMaybes $ fmap stringToPieceField [p ++ f | (p, f) <- collected]
    where
         expanded = concat $ fmap asRepeated $ cleanFenString s
         pieceStrings = fmap fenPieceFormatter expanded
         collected = zip pieceStrings allFieldsForFen


cleanFenString :: String -> String 
cleanFenString = filter (not . (`elem` ("/"::String)))

allFieldsForFen :: [String]
allFieldsForFen = fmap show [Field c r | r <- reverse allRows, c <- allColumns]

fenColorString :: Char -> Char
fenColorString x
  | x `elem` ['a'..'z'] = 'B'
  | x `elem` ['A'..'Z'] = 'W'
  | otherwise = ' '

fenPieceFormatter :: Char -> String
fenPieceFormatter x = (fenColorString x):[C.toUpper x]

asRepeated :: Char -> String
asRepeated x 
  | x `elem` ['1'..'8'] = take (read [x] :: Int) (repeat '0')
  | otherwise = [x]


parseFen :: Parser GameState
parseFen = do
  positionFen :: String <- many1' (letter <|> digit <|> (char '/'))
  let position = fenStringToPosition positionFen
  space
  playerToMoveString :: Char <- (char 'w' <|> char 'b')
  let playerToMove = fromJust $ colorString [C.toUpper playerToMoveString]
  space
  castlingRightsString :: String <- many1' (char 'K' <|> char 'Q' <|> char 'q' <|> char 'k')
  let castlingRights = castlingRightsParser castlingRightsString
  space
  epTargetString :: String <- many1' (letter <|> digit <|> char '-')
  let epTarget = stringToField $ fmap C.toUpper epTargetString
  space
  halfMove :: Int <- decimal
  space
  fullMove :: Int <- decimal
  endOfInput
  -- let halfMove = 1
  -- let fullMove = 1
  return $ GameState position playerToMove castlingRights epTarget halfMove fullMove


castlingRightsParser :: String -> CastlingRights
castlingRightsParser s = ((wk, wq), (bk, bq))
  where
    wk = 'K' `elem` s
    bk = 'k' `elem` s
    wq = 'Q' `elem` s
    bq = 'q' `elem` s

-- sortedFields = [(col, row) | col <- allColumns, row <- allRows]

moveDistance (from, to) = abs (fromX - toX) + abs (fromY - toY)
    where (fromX, fromY) = fieldToInt from
          (toX, toY) = fieldToInt to

