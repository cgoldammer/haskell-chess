module Chess.Logic (allPhysicalMoves, allPiecePhysicalMoves
            , GameState (..), gsPosition, gsColor, gsCastlingRights, gsEnPassantTarget
            , CastlingRights (..), PlayerData (..), CastlingData (..)
            , castleNone, castleAll
            , defaultGameState, defaultGameStateNoCastle
            , startingGS
            , getPositions
            , invertGameStateColor
            , ownPieceFields
            , allNextLegalMoves, allStandardMoves, opponentCount, opponentNum, allControllingFields
            , filterOutInCheck
            , parsePromotionPiece
            , move, move', updatePositionMove, tryMoves, movePiece
            , isMate
            , basicFen
            , fullFen
            , isChecking, isTaking
            , filterOutInCheckFull, checkInRoute, isCheckInRoute, gameStateRoutes, routeData, returnCheckingRoute
            , inCheck
            , pieceFields
            , pieceFieldForMove
            , parseFen, fenToGameState, gameStateToFen
            , castlingRightsParser, freeForCastling
            , allOpponentMoves
            , fenStringToPosition
            , stringToMove
            , a1, b1, c1, d1, e1, f1, g1, h1, a8, b8, c8, d8, e8, f8, g8, h8
            , MoveReader
            , Game (..)
            , gameFromString, gameFromStart
            ) where

import Chess.Board
import Chess.Helpers

import Debug.Trace
import Data.Maybe
import Control.Lens as L
import Control.Monad hiding ((^.))
import Control.Applicative hiding ((^.))
import qualified Data.String.Utils as SU
import qualified Data.Char as C
import qualified Data.Text as Te
import qualified Text.Read as TR
import Data.Attoparsec.Text hiding (take, D, takeWhile)
import Data.Attoparsec.Combinator hiding ((^.))
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.Either.Combinators as EitherC
import Data.List
import System.IO.Unsafe

-- The following fields are often used as part of the chess logic, so I'm defining
-- them as variables and exporting them.
rowFields row = fmap (fromJust . stringToField) [c : (show row) | c <- ['A'..'H']]
[a1, b1, c1, d1, e1, f1, g1, h1] = rowFields 1 
[a8, b8, c8, d8, e8, f8, g8, h8] = rowFields 8

data PlayerData a = PlayerData {
    dataWhite :: a
  , dataBlack :: a
} deriving (Eq, Show)

getPlayerData :: PlayerData a -> Color -> a
getPlayerData playerData White = dataWhite playerData
getPlayerData playerData Black = dataBlack playerData

data CastlingData a = CastlingData {
    canKingSide :: a
  , canQueenSide :: a
} deriving (Eq, Show)

instance Functor CastlingData where
  fmap f (CastlingData king queen) = CastlingData (f king) (f queen)

type CastleKingSide = Bool

getCastlingData :: CastlingData a -> CastleKingSide -> a
getCastlingData castlingData True = canKingSide castlingData
getCastlingData castlingData False = canQueenSide castlingData

canCastle :: (CastlingData Bool) -> Bool
canCastle (CastlingData False False) = False
canCastle _ = True

type CastlingRights = PlayerData (CastlingData Bool)

-- | A `GameState` describes the current game position fully. It contains the same information as the
-- Fen notation <https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation>.
data GameState = GameState {
      _gsPosition :: Position
    , _gsColor :: Color
    , _gsCastlingRights :: CastlingRights
    , _gsEnPassantTarget :: Maybe Field
    , _gsHalfMove :: Int
    , _gsFullMove :: Int} deriving (Eq, Show)
makeLenses ''GameState


castleAll = PlayerData (CastlingData True True) (CastlingData True True)

defaultGameState :: Position -> Color -> GameState
defaultGameState ps color = GameState ps color castleAll Nothing 0 1

castleNone = PlayerData (CastlingData False False) (CastlingData False False)

defaultGameStateNoCastle :: Position -> Color -> GameState
defaultGameStateNoCastle ps color = GameState ps color castleNone Nothing 0 1

startGameString = [
    "WRA1", "WNB1", "WBC1", "WQD1", "WKE1", "WBF1", "WNG1", "WRH1"
  , "WPA2", "WPB2", "WPC2", "WPD2", "WPE2", "WPF2", "WPG2", "WPH2"
  , "BPA7", "BPB7", "BPC7", "BPD7", "BPE7", "BPF7", "BPG7", "BPH7"
  , "BRA8", "BNB8", "BBC8", "BQD8", "BKE8", "BBF8", "BNG8", "BRH8"]
startingGS = defaultGameState (fmap (fromJust . stringToPieceField) startGameString) White


movePiece :: GameState -> Move -> Piece
movePiece gs mv = (head matchingPieces) ^. pfPiece
  where from = mv ^. moveFrom
        matchingPieces = selectByPosition (gs ^. gsPosition) [from]
        

tryMoves :: Maybe GameState -> [Move] -> Maybe GameState
tryMoves Nothing _ = Nothing
tryMoves (Just gs) [] = Just gs
tryMoves (Just gs) (mv : rest) = if isLegalMove then tryMoves (Just (move gs piece mv)) rest else Nothing
  where piece = movePiece gs mv
        isLegalMove = elem mv $ fmap snd $ allNextLegalMoves gs

type IsTaking = Bool
type IsPawnMove = Bool

updateFullMove :: Color -> Int -> Int
updateFullMove White n = n
updateFullMove Black n = n + 1

updateHalfMove :: GameState -> Move -> Int -> Int
updateHalfMove _ (EnPassantMove _ _ _) _ = 1
updateHalfMove gs mv@(StandardMove from to) n = if (isTakingMove || isPawnMove) then 1 else updateFullMove color n
  where isTakingMove = isTaking gs to
        isPawnMove = movePiece gs mv == Pawn
        color = gs ^. gsColor

getPositionChange :: GameState -> Piece -> Move -> ([Field], [PieceField])
getPositionChange gs piece (StandardMove from to) = (removePf, addPf)
  where position = gs ^. gsPosition
        removePf = [from]
        color = gs ^. gsColor
        addPf = [PieceField piece color to]
getPositionChange gs Pawn (PromotionMove from to piece) = (removePf, addPf)
  where position = gs ^. gsPosition
        removePf = [from]
        color = gs ^. gsColor
        addPf = [PieceField piece color to]
getPositionChange gs King (CastlingMove from to rookFrom rookTo) = (removePf, addPf)
  where position = gs ^. gsPosition
        removePf = [from, rookFrom]
        color = gs ^. gsColor
        addPf = [PieceField King color to, PieceField Rook color rookTo]
getPositionChange gs Pawn (EnPassantMove from to pawnCapturedField) = (removePf, addPf)
  where position = gs ^. gsPosition
        color = gs ^. gsColor
        fieldTaken = fromJust $ shiftRowIntoOwnDirection pawnCapturedField color 1
        removePf = [from, fieldTaken]
        addPf = [PieceField Pawn color to]

shiftRowIntoOwnDirection :: Field -> Color -> Int -> Maybe Field
shiftRowIntoOwnDirection (Field column row) color number = Field <$> Just column <*> takenRow
  where rowNumber = rowInt row
        rowChange = if (color == White) then (-number) else (number)
        takenRow = intRow $ rowNumber + rowChange

removeFieldFromPosition :: Position -> Field -> Position
removeFieldFromPosition position field = [pf | pf <- position, pf ^. pfField /= field]

updatePosition :: Position -> [Field] -> [PieceField] -> Position
updatePosition position remove add = (foldl removeFieldFromPosition position remove) ++ add


move :: GameState -> Piece -> Move -> GameState
move gs piece mv = GameState newPosition newColor newCastlingRights newEnPassant newHalfMove newFullMove
  where   oldColor = gs ^. gsColor
          newColor = invertColor oldColor
          oldPosition = gs ^. gsPosition
          newPosition = (uncurry (updatePosition oldPosition)) $ getPositionChange gs piece mv
          newHalfMove = updateHalfMove gs mv (gs ^. gsHalfMove)
          newFullMove = updateFullMove oldColor (gs ^. gsFullMove)
          newCastlingRights = updateCastlingRights mv (gs ^. gsCastlingRights)
          newEnPassant = updateEnPassant gs mv

move' :: GameState -> Move -> GameState
move' gs mv = move gs (movePiece gs mv) mv

-- | This is a helper function that just updates the gamestate position, but not logic like
-- castling rights. It is vastly faster, and used to determine whether I'm moving into a check.
updatePositionMove :: GameState -> Piece -> Move -> GameState
updatePositionMove gs piece mv = GameState newPosition newColor oldCastlingRights oldEp oldHalfMove oldFullMove
  where   oldColor = gs ^. gsColor
          newColor = invertColor oldColor
          oldPosition = gs ^. gsPosition
          newPosition = (uncurry (updatePosition oldPosition)) $ getPositionChange gs piece mv
          oldCastlingRights = gs ^. gsCastlingRights
          oldHalfMove = gs ^. gsHalfMove
          oldFullMove = gs ^. gsFullMove
          oldEp = gs ^. gsEnPassantTarget


-- updatePosition :: Position -> Field -> Field -> Position
-- updatePosition ps field newField = newPf : [p | p <- ps, p /= pf]
--     where pf = head $ filter (\p -> p ^. pfField == field) ps
--           newPf = PieceField piece color newField
--           piece = pf ^. pfPiece
--           color = pf ^. pfColor


updateEnPassant :: GameState -> Move -> Maybe Field
updateEnPassant gs mv@(StandardMove from@(Field fromC fromR) to@(Field toC toR))
  | pawnMovedTwo = beforeField 
  | otherwise = Nothing
  where
      pawnMovedTwo = movedPawn && distance == 2 && fromC == toC
      movedPawn = movePiece gs mv == Pawn
      distance = moveDistance (from, to)
      beforeRow = nextRow fromR (_gsColor gs)
      beforeField = liftM2 Field (Just fromC) beforeRow
updateEnPassant gs _ = Nothing

nextRow :: Row -> Color -> Maybe Row
nextRow r White = intRow $ (rowInt r) + 1
nextRow r Black = intRow $ (rowInt r) - 1

updateCastlingRights :: Move -> CastlingRights -> CastlingRights
updateCastlingRights (CastlingMove from to rookFrom rookTo) castlingRights = newRights
  where whiteK = not (from == e1 && rookFrom == h1)
        whiteQ = not (from == e1 && rookFrom == a1)
        blackK = not (from == e8 && rookFrom == h8)
        blackQ = not (from == e8 && rookFrom == a8)
        PlayerData (CastlingData whiteKBefore whiteQBefore) (CastlingData blackKBefore blackQBefore) = castlingRights
        newRightsWhite = CastlingData (whiteKBefore && whiteK) (whiteQBefore && whiteQ)
        newRightsBlack = CastlingData (blackKBefore && blackK) (blackQBefore && blackQ)
        newRights = PlayerData newRightsWhite newRightsBlack
updateCastlingRights _ castlingRights = castlingRights
  
pieceFieldForMove :: GameState -> Move -> PieceField
pieceFieldForMove gs mv = head [pf | pf <- gs ^. gsPosition, mv ^. moveFrom == pf ^. pfField]

allNextStates :: GameState -> [GameState]
allNextStates gs = fmap (uncurry (move gs)) (allNextLegalMoves gs)

isMate :: GameState -> Bool
isMate gs = noNextMoves && inCheck gs
    where noNextMoves = length (allNextLegalMoves gs) == 0

allOpponentMoves :: GameState -> [(Piece, Move)]
allOpponentMoves = allPhysicalMoves . invertGameStateColor

invertGameStateColor :: GameState -> GameState
invertGameStateColor gs = over gsColor invertColor gs

allStandardMoves :: GameState -> [Move]
allStandardMoves gs = concat $ fmap (allStandardPhysicalMoves gs) (ownPieceFields gs)

allNextLegalMoves :: GameState -> [(Piece, Move)]
allNextLegalMoves gs = filterOutInCheck gs $ allPhysicalMoves gs
    
inCheck :: GameState -> Bool
inCheck gs@(GameState ps color cr ept hm fm) = (ownKingField gs) `elem` opponentFields
  where opponentFields = fmap (L.view moveTo) $ allStandardMoves . invertGameStateColor $ gs

isChecking :: GameState -> Bool
isChecking = inCheck . invertGameStateColor
            
notInCheck = not . inCheck

filterOutInCheckFull :: GameState -> [(Piece, Move)] -> [(Piece, Move)]
filterOutInCheckFull gs pms = filter notInCheck pms
    where notInCheck (piece, mv) = not $ isChecking $ updatePositionMove gs piece mv

ownKingField gs = head $ fmap (view pfField) $ filter ((==King) . (view pfPiece)) $ ownPieceFields gs

filterOutInCheck :: GameState -> [(Piece, Move)] -> [(Piece, Move)]
filterOutInCheck gs moves = legalKingMoves ++ filteredOtherMoves
  where (kingMoves, otherMoves) = partition (\(p, m) -> p == King) moves
        kingField = ownKingField gs
        allOpponentControlledFields = allControllingFields $ invertGameStateColor gs
        filterControlling = \(_, m) -> not ((m ^. moveTo) `elem` allOpponentControlledFields)
        legalKingMoves = filter filterControlling kingMoves
        routes = gameStateRoutes gs
        routeFilter = \(_, m) -> not (isCheckInRoute m routes)
        filteredOtherMoves = filter routeFilter otherMoves
        

-- All opponent pieces that could possibly check the king in the current position,
-- assuming a piece moves.
data CheckingRoute = CheckingRoute { routePiece :: PieceField, routeBetweenFields :: [Field], routeBetweenPiece :: Maybe PieceField, routeKingPosition :: Field} deriving Show

checkInRoute :: CheckingRoute -> Move -> Bool
checkInRoute cr mv = (not inCheck && creatingCheck) || (inCheck && not preventingCheck)
  where creatingCheck = movingPieceInBetween && not pieceIsBetweenAfterMove && not takingPiece
        preventingCheck = inCheck && (pieceIsBetweenAfterMove || takingPiece)
        takingPiece = to == (routePiece cr) ^. pfField
        pieceIsBetweenAfterMove = to `elem` betweenFields
        (to, from) = (mv ^. moveTo, mv ^. moveFrom)
        betweenFields = routeBetweenFields cr
        betweenPiece = routeBetweenPiece cr
        inCheck = not $ isJust $ routeBetweenPiece cr
        movingPieceInBetween = Just from == fmap (view pfField) betweenPiece
    
isCheckInRoute :: Move -> [CheckingRoute] -> Bool
isCheckInRoute mv = any $ (flip checkInRoute) mv

-- A route goes either in row or column from the king position.
-- The maximum number of own pieces on it is 1
-- The first opponent piece on it must be able to check.
--

routeData :: GameState -> ([PieceField], [PieceField], Field, [(Bool, [Field])])
routeData gs = (own, opp, kingField, allMoves)
  where kingField = ownKingField gs
        rookMoves = [(True, pf) | pf <- pieceFields (PieceField Rook White kingField)]
        bishopMoves = [(False, pf) | pf <-  pieceFields (PieceField Bishop White kingField)]
        allMoves = filter (\m -> length (snd m) > 0) $ bishopMoves ++ rookMoves
        own = ownPieceFields gs
        opp = opponentPieceFields gs

gameStateRoutes :: GameState -> [CheckingRoute]
gameStateRoutes gs = catMaybes $ fmap (returnCheckingRoute own opp kingField) allMoves
  where (own, opp, kingField, allMoves) = routeData gs

-- go through the fields. For each field, accumulate the count of own and opponent pieces
-- Only look for rooks, bishops, and queens
-- Stop when own count > 1 or opponent count > 1
-- If opponent count == 0 at end, return Nothing
returnCheckingRoute :: [PieceField] -> [PieceField] -> Field -> (Bool, [Field]) -> Maybe CheckingRoute
returnCheckingRoute _ _ _ (_, []) = Nothing
returnCheckingRoute own opp kingField (isLine, fields) = makeMaybe correct $ CheckingRoute firstOppPieceField beforeOpp firstOwnPieceField kingField
  where (before, after) = span (fmap not isOpponentPiece) $ zip fields (fmap indicator fields)
        (beforeOpp, afterOpp) = (fmap fst before, fmap fst after)
        indicator = pieceIndicator ownFields oppFields
        oppCount = length afterOpp
        firstOpp = head afterOpp
        hasOpp = oppCount > 0 && firstOpp `elem` oppFields
        firstOppPieceField = head $ filter (\pf -> pf ^. pfField == firstOpp) opp
        [ownFields, oppFields] = (fmap . fmap) (view pfField) [own, opp]
        ownFieldsInList = dropWhile (\f -> not (f `elem` beforeOpp)) ownFields
        firstOwnPiece = safeHead ownFieldsInList -- Maybe Field
        ownPieceOnField f = safeHead $ filter ((==f) . (view pfField)) own -- Field -> Maybe PieceField
        firstOwnPieceField = join $ fmap ownPieceOnField firstOwnPiece -- Maybe PieceField
        oppPiece = firstOppPieceField ^.pfPiece
        oppPieceRightType = (isLine && oppPiece `elem` [Rook, Queen]) || (not isLine && oppPiece `elem` [Bishop, Queen])
        numberOwnFieldsInBefore = length $ filter (\pf -> pf ^. pfField `elem` beforeOpp) own
        correct = hasOpp && oppPieceRightType && (numberOwnFieldsInBefore <= 1)

isOpponentPiece (_, (own, opp)) = opp

pieceIndicator :: [Field] -> [Field] -> Field -> (Bool, Bool)
pieceIndicator own opp f = (f `elem` own, f `elem` opp)

type MoveDirections = [Move]

allControllingFields :: GameState -> [Field]
allControllingFields gs = nonPawnFields ++ pawnFields
  where nonPawnFields = concat $ fmap (allControllingFieldsHelper gs) nonPawns
        (pawns, nonPawns) = partition ((==Pawn) . (view pfPiece)) $ ownPieceFields gs
        color = gs ^. gsColor
        pawnFields = concat $ fmap (pawnTakingFields color . (view pfField)) pawns

pawnTakingFields :: Color -> Field -> [Field]
pawnTakingFields color (Field col row) = catMaybes [left, right]
  where rowChange = if color == White then 1 else (-1)
        colNum = columnInt col
        rowNum = rowInt row
        newRowNum = rowNum + rowChange
        newRow = intRow newRowNum
        leftCol = intColumn (colNum - 1)
        rightCol = intColumn (colNum + 1)
        left = liftA2 Field leftCol newRow
        right = liftA2 Field rightCol newRow

allControllingFieldsHelper :: GameState -> PieceField -> [Field]
allControllingFieldsHelper gs@(GameState position color _ _ _ _) pf@(PieceField piece _ field) = fmap snd goodMoves
    where fields = pieceFields pf
          withCount = fmap (opponentNum (fmap (view pfField) position)) fields
          goodFields = concat $ (fmap . fmap) fst $ fmap (takeWhile (\(_, c) -> c <= 1)) withCount
          goodMoveFields = [(from, to) | (from, to) <- zip (repeat field) goodFields] :: [MoveLocation]
          goodMoveFilterPawn = filterPawnMoves gs piece goodMoveFields
          goodMoves = goodMoveFilterPawn
          
allOpponentFields gs = fmap (view pfField) $ ownPieceFields $ invertGameStateColor gs

allStandardPhysicalMoves :: GameState -> PieceField -> [Move]
allStandardPhysicalMoves gs pf@(PieceField piece _ field) = if pawnPromotes then promotionMoves else standardMoves
  where fields = pieceFields pf
        withCount = fmap (opponentNum opponentFields) fields
        notOwn f = not $ f `elem` (fmap (view pfField) (ownPieceFields gs))
        goodFields = concat $ fmap (takeWhile notOwn) $ (fmap . fmap) fst $ fmap (takeWhile (\(_, c) -> c <= 1)) withCount
        opponentFields = allOpponentFields gs
        goodMoveFields = [(from, to) | (from, to) <- zip (repeat field) goodFields] :: [MoveLocation]
        completeGoodFields = if piece == Pawn then [ml | ml <- goodMoveFields, isLegalPawnMove gs ml] else goodMoveFields
        promotionRow = if (gs ^. gsColor) == White then R7 else R2
        pawnPromotes = (piece == Pawn) && ((field ^. fieldRow) == promotionRow)
        standardMoves = fmap (uncurry StandardMove) completeGoodFields
        promotionMoves = concat $ fmap addPromotionMoves completeGoodFields
        
addPromotionMoves :: MoveLocation -> [Move]
addPromotionMoves (from, to) = [PromotionMove from to piece | piece <- allNonKingFullPieces]


opponentCount :: Eq a => [a] -> [a] -> Int -> [(a, Int)]
opponentCount fs [] n = zip fs (repeat n)
opponentCount (f:rest) opfs n = (f, nextNum) : opponentCount rest opfs nextNum
  where nextNum = if (f `elem` opfs ||  n >= 1) then n + 1 else n
opponentCount [] _ _ = []

opponentNum :: Eq a => [a] -> [a] -> [(a, Int)]
opponentNum f f' = opponentCount f' f 0

allPiecePhysicalMoves :: GameState -> PieceField -> [(Piece, Move)]
allPiecePhysicalMoves gs@(GameState position color _ _ _ _) pf@(PieceField piece _ field) = zip (repeat piece) (goodMoves ++ castMoves)
    where castMoves = if piece == King then possibleCastlingMoves gs else []
          goodMoves = allStandardPhysicalMoves gs pf

filterPawnMoves :: GameState -> Piece -> [MoveLocation] -> [MoveLocation]
filterPawnMoves gs Pawn ml = filter (isLegalPawnMove gs) ml
filterPawnMoves _ _ ml = ml

isLegalPawnMove :: GameState -> MoveLocation -> Bool
isLegalPawnMove gs ml@(from, to) = (isGoingForward && notTaking) || ((not isGoingForward) && (taking || isEP))
    where   isGoingForward = to ^. fieldColumn == from ^. fieldColumn
            taking = isTaking gs to
            notTaking = not taking
            isEP = isEnPassant (_gsEnPassantTarget gs) Pawn (from, to)

isTaking :: GameState -> Field -> Bool
isTaking gs to = elem to $ fmap (view pfField) (opponentPieceFields gs)

freeForCastling gs = if (inCheck gs) then [] else [kingCurrent && kingFields, queenCurrent && queenFields]
  where occupied = fmap (view pfField) $ _gsPosition gs
        controlled = allControllingFields $ invertGameStateColor gs
        color = gs ^. gsColor
        castlingFieldsForPlayer = getPlayerData castlingFields color -- CastlingData Bool
        currentRights = getPlayerData (gs ^. gsCastlingRights) color
        CastlingData kingCurrent queenCurrent = currentRights
        rightsBasedOnFields = fmap (getCastleOption (occupied, controlled)) castlingFieldsForPlayer
        CastlingData kingFields queenFields = rightsBasedOnFields

castlingFieldsWhite = CastlingData ([f1, g1], [f1, g1]) ([d1, c1, b1], [d1, c1, b1])
castlingFieldsBlack = CastlingData ([f8, g8], [f8, g8]) ([d8, c8, b8], [d8, c8, b8])
castlingFields = PlayerData castlingFieldsWhite castlingFieldsBlack

castlingMoves :: GameState -> [Move]
castlingMoves gs
  | _gsColor gs == White = [CastlingMove e1 g1 h1 f1, CastlingMove e1 c1 a1 d1]
  | _gsColor gs == Black = [CastlingMove e8 g8 h8 f8, CastlingMove e8 c8 a8 d8]
    
possibleCastlingMoves :: GameState -> [Move]
possibleCastlingMoves gs = catMaybes [makeMaybe c m | (m, c) <- zip (castlingMoves gs) (freeForCastling gs)]

-- hasCastlingRight :: Color -> CastleKingSide -> CastlingRights -> Bool
-- hasCastlingRight color castleKingSide castlingRights = getCastlingData castleKingSide $ getPlayerData color castlingRights

noBadMove :: [Field] -> [Field] -> Bool
noBadMove badFields fields = not $ any (\f -> f `elem` badFields) fields

type OccupiedField = Field
type ControlledByOpponentField = Field
type ShouldNotBeOccupiedField = Field
type ShouldNotBeControlledField = Field

getCastleOption :: ([OccupiedField], [ControlledByOpponentField]) -> ([ShouldNotBeOccupiedField], [ShouldNotBeControlledField]) -> Bool
getCastleOption (occupied, controlled) (mustNotBeOccupied, mustNotBeControlled) = noBadMove occupied mustNotBeOccupied && noBadMove controlled mustNotBeControlled

freeField :: GameState -> Field -> Bool
freeField gs f = not $ elem f $ fmap (view pfField) $ _gsPosition gs

isEnPassant :: Maybe Field -> Piece -> MoveLocation -> Bool
isEnPassant ept piece (from, to) = piece == Pawn && ept == Just to

ownPieceFields :: GameState -> [PieceField]
ownPieceFields gs = filter (\pf -> pf ^. pfColor == _gsColor gs) (_gsPosition gs)

opponentPieceFields :: GameState -> [PieceField]
opponentPieceFields = ownPieceFields . invertGameStateColor 

allPhysicalMoves :: GameState -> [(Piece, Move)]
allPhysicalMoves gs = concat $ fmap (allPiecePhysicalMoves gs) (ownPieceFields gs)

fieldStep :: Field -> (Int, Int) -> Maybe Field
fieldStep (Field c r) (x, y) = makeMaybe allLegit $ Field newCol newRow
    where   cInt = columnInt c
            rInt = rowInt r
            (newX, newY) = (cInt + x, rInt + y)
            (newCol, newRow) = (fromJust (intColumn newX), fromJust (intRow newY))
            allLegit = isJust (intColumn newX) && isJust (intRow newY)

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
pawnSteps (Field _ R2) White = [[(-1, 1)], [(0, 1), (0, 2)], [(1, 1)]]
pawnSteps (Field _ _) White = [[(-1, 1)], [(0, 1)], [(1,1)]]
pawnSteps (Field _ R7) Black = [[(-1, -1)], [(0, -1), (0, -2)], [(1, -1)]]
pawnSteps _ Black = [[(-1, -1)], [(0, -1)], [(1, -1)]]

allPieceFields :: PieceField -> [Field]
allPieceFields = concat . pieceFields

getPositions :: GameState -> Piece -> [Field]
getPositions gs pc = fmap (view pfField) $ filter isRightPiece position
    where   isRightPiece (PieceField pfP pfC field) = (pfP == pc) && (pfC == color)
            color = _gsColor gs
            position = _gsPosition gs
            
selectByPosition :: Position -> [Field] -> Position
selectByPosition ps fs = filter (\pf -> (elem (pf ^. pfField) fs)) ps

type Fen = String

pieceFieldFen :: Maybe PieceField -> Char
pieceFieldFen Nothing = '1'
pieceFieldFen (Just (PieceField piece color field)) = pieceChar
    where   pieceChar = transformer pieceLetter
            pieceLetter = (showPiece piece) !! 0
            transformer 
                | color == White = id
                | color == Black = C.toLower

piecesOnField :: Position -> Field -> Maybe PieceField
piecesOnField ps f = safeIndex 0 (filter byField ps)
    where   byField pf = pf ^. pfField == f

basicFenFromRow :: [PieceField] -> String
basicFenFromRow ps = fmap (\c -> pieceFieldFen (firstPieceOnColumn ps c)) allColumns

aggregateFen :: Int -> String -> String
aggregateFen i = SU.replace (take i (repeat '1')) (show i)

piecesOnRow :: Position -> Row -> Position
piecesOnRow ps r = filter (\pf -> pf ^. pfField . fieldRow == r) ps

firstPieceOnColumn :: Position -> Column -> Maybe PieceField
firstPieceOnColumn ps c = safeIndex 0 (filter (\pf -> pf ^. pfField . fieldColumn == c) ps)

positionByRow :: Position -> [Position]
positionByRow ps = fmap (piecesOnRow ps) (reverse allRows)

basicFen :: Position -> Fen
basicFen ps = intercalate "/" $ fmap (aggregator . basicFenFromRow) $ positionByRow ps

aggregator s = foldl (flip ($)) s (fmap aggregateFen (reverse [1..8]))

gameStateToFen :: GameState -> Fen
gameStateToFen gs@(GameState ps color cr ept hm fm) = intercalate " " elements
  where elements = ["fen", positionFen, col, castleString, epString, show hm, show fm]
        positionFen = basicFen ps
        col = fmap C.toLower $ colorToString color 
        castleString = castlingRightsToString cr
        epString = epToString ept

epToString :: Maybe Field -> String
epToString (Just f) = fmap C.toLower $ showField f
epToString Nothing = "-"

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

fenToGameState :: String -> Maybe GameState
fenToGameState = EitherC.rightToMaybe . parseOnly parseFen . Te.pack

parseFen :: Parser GameState
parseFen = do
  string "fen "
  positionFen :: String <- many1' (letter <|> digit <|> (char '/'))
  let position = fenStringToPosition positionFen
  space
  playerToMoveString :: Char <- (char 'w' <|> char 'b')
  let playerToMove = fromJust $ colorString [C.toUpper playerToMoveString]
  space
  castlingRightsString :: String <- many1' (char 'K' <|> char 'Q' <|> char 'q' <|> char 'k' <|> char '-')
  let castlingRights = castlingRightsParser castlingRightsString
  space
  epTargetString :: String <- many1' (letter <|> digit <|> char '-')
  let epTarget = stringToField $ fmap C.toUpper epTargetString
  space
  halfMove :: Int <- decimal
  space
  fullMove :: Int <- decimal
  endOfInput
  return $ GameState position playerToMove castlingRights epTarget halfMove fullMove

castlingRightsParser :: String -> CastlingRights
castlingRightsParser s = PlayerData (CastlingData wk wq) (CastlingData bk bq)
  where
    wk = 'K' `elem` s
    bk = 'k' `elem` s
    wq = 'Q' `elem` s
    bq = 'q' `elem` s

castlingRightsToString :: CastlingRights -> String
castlingRightsToString (PlayerData (CastlingData wk wq) (CastlingData bk bq)) = if length concatenated > 0 then concatenated else "-"
  where wkC = castlingRightsChar wk White True
        wqC = castlingRightsChar wq White False
        bkC = castlingRightsChar bk Black True
        bqC = castlingRightsChar bq Black False
        values = [wkC, wqC, bkC, bqC]
        concatenated = concat values

castlingRightsChar :: Bool -> Color -> Bool -> String
castlingRightsChar True White True = "K"
castlingRightsChar True White False = "Q"
castlingRightsChar True Black True = "k"
castlingRightsChar True Black False = "q"
castlingRightsChar False _ _ = ""

moveDistance (from, to) = abs (fromX - toX) + abs (fromY - toY)
    where (fromX, fromY) = fieldToInt from
          (toX, toY) = fieldToInt to

stringToCastleMove  :: String -> Maybe Move
stringToCastleMove "E1G1" = Just $ CastlingMove e1 g1 h1 f1
stringToCastleMove "E8G8" = Just $ CastlingMove e8 g8 h8 f8
stringToCastleMove "E1C1" = Just $ CastlingMove e1 c1 a1 d1
stringToCastleMove "E8C8" = Just $ CastlingMove e8 c8 a8 d8
stringToCastleMove _ = Nothing

currentPlayerCanCastle :: GameState -> Bool
currentPlayerCanCastle gs = canCastle $ getPlayerData (gs ^. gsCastlingRights) (gs ^. gsColor)

isCastleMove :: GameState -> String -> Bool
isCastleMove gs mv = (mv `elem` ["E1G1", "E8G8", "E1C1", "E8C8"]) && currentPlayerCanCastle gs


stringToMove :: MoveReader
stringToMove gs mv
  | isCastleMove gs mv = (,) <$> Just King <*> stringToCastleMove mv
  | otherwise = nonCastleStringToMove gs mv

parsePromotionPiece :: String -> Maybe Piece
parsePromotionPiece p
  | length p == 1 && (head p) `elem` ("QRBN" :: String) = stringToPiece p
  | otherwise = Nothing

nonCastleStringToMove :: GameState -> String -> Maybe (Piece, Move)
nonCastleStringToMove gs (c1 : c2 : c3 : c4 : rest) = (,) <$> fromPiece <*> move
  where from = stringToField [c1, c2]
        to = stringToField [c3, c4] 
        promotionPiece = parsePromotionPiece rest
        fromPiece = safeHead [pf ^. pfPiece | pf <- gs ^. gsPosition, Just (pf ^. pfField) == from]
        epTarget = gs ^. gsEnPassantTarget
        isEp = epTarget == to && fromPiece == Just Pawn
        moveEnPassant = EnPassantMove <$> from <*> to <*> epTarget
        movePromotion = PromotionMove <$> from <*> to <*> promotionPiece
        moveStandard = StandardMove <$> from <*> to
        move = if (not isEp) then (if isJust promotionPiece then movePromotion else moveStandard) else moveEnPassant

type MoveReader = GameState -> String -> Maybe (Piece, Move)

type FullState = (Maybe GameState, Maybe (Piece, Move))

pgnMoveFolder :: MoveReader -> FullState -> String -> FullState
pgnMoveFolder mr (Nothing, _) _ = (Nothing, Nothing)
pgnMoveFolder mr (Just gs, _) pgnMove = (gs', mv)
  where mv = mr gs pgnMove
        gs' = fmap (uncurry (move gs)) mv

gsFromStringMoves :: MoveReader -> GameState -> [String] -> [FullState]
gsFromStringMoves mr gs pgnMoves = scanl (pgnMoveFolder mr) (Just gs, Nothing) pgnMoves

gsFromStringMovesStart :: MoveReader -> [String] -> [FullState]
gsFromStringMovesStart mr mvs = gsFromStringMoves mr startingGS mvs

-- foldGame :: MoveReader -> Either String Game -> String -> Either Game
-- foldGame = undefined
-- foldGame = mr (Left err) _ = Left err
-- foldGame mr (Right game) mvString = 
--   where gs = tail $ gameStates game
--         mv = mr gs mvString -- Maybe (Piece, Move)
--         newGs = fmap (uncurry (move gs)) mv -- Maybe GameState
--         newData = undefined -- Maybe (GameState, Move)
--         movesSoFar = gameMoves game
--         gameMoveNumber = div (length movesSoFar) 2
--         error = "Parsed moves until " ++ show gameMoveNumber ++ show movesSoFar


-- foldGameEither :: Either String [Move] -> Maybe Move -> Either String [Move] 
-- foldGameEither (Right mvs) Nothing = Left $ "Parsed moves until " ++ show (div (length mvs) 2) ++ show mvs
-- foldGameEither (Right mvs) (Just mv) = Right $ mvs ++ [mv]
-- foldGameEither (Left err) _ = Left err

-- | A `Game` consists of a starting game state and a list of moves.
-- Todo: This implementation allows creating a game with illegal moves.
-- I should aim to prevent this using a newtype.
data Game = Game { 
    startingGameState :: GameState
  , gameStates :: [GameState]
  , gameMoves :: [Move] } deriving (Show)

createGameFromAccum :: GameState -> ([GameState], [Move]) -> Game
createGameFromAccum startingGs (gs, mvs) = Game startingGs (reverse gs) (reverse mvs)

gameFromString :: MoveReader -> GameState -> [String] -> Either String Game
gameFromString mr startingGs stringMoves = fmap (createGameFromAccum startingGs) gameAccum
  where gameAccum = gameFromStringAccum mr (Right ([startingGs], [])) stringMoves

gameFromStringAccum :: MoveReader -> Either String ([GameState], [Move]) -> [String] -> Either String ([GameState], [Move])
gameFromStringAccum mr (Left error) _ = Left error
gameFromStringAccum mr (Right (gsList, mvList)) [] = Right (gsList, mvList)
gameFromStringAccum mr (Right (currentGs : restGs, mvList)) (nextStringMove : restStringMoves) = case parsedMove of
          Just (gs, m) -> gameFromStringAccum mr (Right ((gs : currentGs : restGs), (m : mvList))) restStringMoves
          Nothing         -> Left error
  where gameMoveNumber = div (length mvList) 2
        error = "Parsed moves until " ++ show gameMoveNumber ++ show mvList
        parsedMove = tryParsingMove mr currentGs nextStringMove -- Maybe (GameState, Piece, Move)

tryParsingMove :: MoveReader -> GameState -> String -> Maybe (GameState, Move)
tryParsingMove mr gs stringMove = (,) <$> executedMove <*> (fmap snd parsedMove)
  where parsedMove = mr gs stringMove -- Maybe (Piece, Move)
        executedMove = fmap (uncurry (move gs)) parsedMove -- Maybe GameState
    
gameFromStart :: MoveReader -> [String] -> Either String Game
gameFromStart mr mvs = gameFromString mr startingGS mvs

