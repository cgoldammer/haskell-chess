
-- Pawns, promotion, castling


module Logic (allPhysicalMoves
            , GameState (GameState, gsPosition)
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
            , allOpponentMoves
            ) where

import Data.Maybe
import Board
import qualified Data.String.Utils as SU
import qualified Data.Char as C
import Helpers
import Data.List
-- Todo
-- Pawns (1 vs 2 moves, capturing diagonally, en passant, promotion)
-- Castling


data GameState = GameState {
      gsPosition :: Position
    , gsColor :: Color
    , castlingRights :: ((Bool, Bool), (Bool, Bool))
    , enPassantTarget :: Maybe Field
    , halfMove :: Int
    , fullMove :: Int} deriving (Eq, Show)

defaultGameState :: Position -> Color -> GameState
defaultGameState ps color = GameState ps color ((False, False), (False, False)) Nothing 0 1

move :: GameState -> Move -> GameState
move gs (Move from to promotion) = GameState newPosition newColor cr ept hm fm
    where   newColor = invertColor oldColor
            oldColor = gsColor gs
            oldPosition = gsPosition gs
            removePosition = filter (\pf -> not (elem (pfField pf) [from, to])) oldPosition
            movedPiece = pfPiece $ (selectByPosition oldPosition [from] !! 0)
            newPosition = removePosition ++ [PieceField movedPiece oldColor to]
            cr = castlingRights gs
            ept = enPassantTarget gs
            hm = halfMove gs
            fm = fullMove gs

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
invertGameStateColor (GameState position color cr ept hm fm) = GameState position (invertColor color) cr ept hm fm
    
inCheck :: GameState -> Bool
inCheck gs = kingPosition `elem` opponentMoves
    where opponentMoves = fmap moveTo $ allOpponentMoves gs
          kingPosition = (getPositions gs King) !! 0

isChecking :: GameState -> Bool
isChecking = inCheck . invertGameStateColor
            

type MoveDirections = [Move]

allPiecePhysicalMoves :: GameState -> PieceField -> [Move]
allPiecePhysicalMoves gs pf = goodMoves ++ castMoves
    where fields = pieceFields pf
          castMoves = fmap normalMove $ castlingMoves gs
          goodFields = (concat $ fmap (takeWhile isGood) fields) :: [Field]
          goodMoveFields = [(from, to) | (from, to) <- zip (repeat field) goodFields] :: [MoveLocation]
          goodMoveFilterPawn = filterPawnMoves gs piece goodMoveFields
          goodMoves = concat $ fmap (addSpecialMoves piece) goodMoveFilterPawn
          isGood f = not $ f `elem` (fmap pfField (ownPieceFields gs))
          piece = pfPiece pf
          field = pfField pf
          position = gsPosition gs
          color = gsColor gs


filterPawnMoves :: GameState -> Piece -> [MoveLocation] -> [MoveLocation]
filterPawnMoves gs Pawn ml = filter (isLegalPawnMove gs) ml
filterPawnMoves _ _ ml = ml

normalMove :: MoveLocation -> Move
normalMove (from, to) = Move from to Nothing

isLegalPawnMove :: GameState -> MoveLocation -> Bool
isLegalPawnMove gs ml@(from, to) = (isGoingForward && notTaking) || (isGoingSideways && (taking || isEP))
    where   isGoingForward = (fst from == fst to) 
            isGoingSideways = (fst from /= fst to)
            taking = isTaking gs to
            notTaking = not taking
            isEP = isEnPassant gs ml

isTaking :: GameState -> Field -> Bool
isTaking gs to = to `elem` (fmap pfField (opponentPieceFields gs))

freeForCastling gs = [canCastleKingSide gs, canCastleQueenSide gs]

castlingMoves gs
    | gsColor gs == White = [((E, R1), (G, R1)), ((E, R1), (E, R1))]
    | gsColor gs == Black = [((E, R8), (G, R8)), ((E, R8), (E, R8))]
    
possibleCastlingMoves :: GameState -> [MoveLocation]
possibleCastlingMoves gs = catMaybes [useValueIfCondition m c | (m, c) <- zip (castlingMoves gs) (freeForCastling gs)]

useValueIfCondition :: a -> Bool -> Maybe a
useValueIfCondition a False = Nothing
useValueIfCondition a True = Just a

notInCheck = not . inCheck

canCastleKingSide :: GameState -> Bool
canCastleKingSide (GameState pos White ((False, _), _)  _ _ _) = False
canCastleKingSide (GameState pos Black ((_, False), _)  _ _ _) = False
canCastleKingSide gs@(GameState pos White ((True, _), _)  _ _ _) = (notInCheck gs) && (castlingFree gs [(F, R1), (G, R1)])
canCastleKingSide gs@(GameState pos Black ((_, True), _)  _ _ _) = (notInCheck gs) && (castlingFree gs [(F, R8), (G, R8)])

canCastleQueenSide :: GameState -> Bool
canCastleQueenSide gs@(GameState pos White (_, (False, _))  _ _ _) = False
canCastleQueenSide gs@(GameState pos Black (_, (_, False))  _ _ _) = False
canCastleQueenSide gs@(GameState pos White (_, (True, _))  _ _ _) = (notInCheck gs) && (castlingFree gs [(D, R1), (C, R1), (B, R1)])
canCastleQueenSide gs@(GameState pos Black (_, (_, True))  _ _ _) = (notInCheck gs) && (castlingFree gs [(D, R8), (C, R8), (B, R8)])

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




addSpecialMoves :: Piece -> MoveLocation -> [Move]
addSpecialMoves Pawn ml@((_, R7), _) = addPromotionMoves ml
addSpecialMoves _ ml@((from, to)) = [Move from to Nothing]

addPromotionMoves :: MoveLocation -> [Move]
addPromotionMoves (from, to) = [Move from to (Just piece) | piece <- allNonKingFullPieces]

specialMoves :: GameState -> PieceField
specialMoves = undefined

ownPieceFields :: GameState -> [PieceField]
ownPieceFields gs = filter (\pf -> (pfColor pf) == (gsColor gs)) (gsPosition gs)

opponentPieceFields :: GameState -> [PieceField]
opponentPieceFields = ownPieceFields . invertGameStateColor 

allPhysicalMoves :: GameState -> [Move]
allPhysicalMoves gs = concat $ fmap (allPiecePhysicalMoves gs) (ownPieceFields gs)

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
pawnSteps (_, R1) _ = []
pawnSteps (_, R8) _ = []
pawnSteps (_, R2) White = [[(-1, 1)], [(0, 1), (0, 2)], [(1, -1)]]
pawnSteps _ White = [[(-1, 1)], [(0, 1)], [(1,1)]]
pawnSteps (_, R7) Black = [[(-1, -1)], [(0, -1), (0, -2)], [(1,- 1)]]
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


type Fen = (String, Int)
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
basicFenFromRow ps = [pieceFieldFen (firstPieceOnColumn ps c) | c <- allColumns]

aggregateFen :: Int -> String -> String
aggregateFen i = SU.replace (take i (repeat '1')) (show i)

piecesOnRow :: Position -> Row -> Position
piecesOnRow ps r = filter (\pf -> snd (pfField pf) == r) ps

firstPieceOnColumn :: Position -> Column -> Maybe PieceField
firstPieceOnColumn ps c = index 0 (filter (\pf -> fst (pfField pf) == c) ps)

positionByRow :: Position -> [Position]
positionByRow ps = fmap (piecesOnRow ps) (reverse allRows)

basicFen :: Position -> String
basicFen ps = intercalate "/" $ fmap aggregator $ fmap basicFenFromRow $ positionByRow ps

fullFen :: Position -> String
fullFen ps = "fen " ++ basicFen ps ++ " w - 0 1"

aggregator s = foldl (flip ($)) s (fmap aggregateFen (reverse [1..8]))




-- sortedFields = [(col, row) | col <- allColumns, row <- allRows]



moveDistance (from, to) = abs (fromX - toX) + abs (fromY - toY)
    where (fromX, fromY) = fieldToInt from
          (toX, toY) = fieldToInt to

