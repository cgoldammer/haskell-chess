{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables, TemplateHaskell #-}
-- Pawns, promotion, castling


module Logic (allPhysicalMoves, allPiecePhysicalMoves
            , GameState (GameState), gsPosition, gsColor
            , CastlingRights, castlingFree
            , defaultGameState, defaultGameStateNoCastle
            , getPositions
            , invertGameStateColor
            , ownPieceFields
            , reachableByBishop, reachableByRook
            , allNextLegalMoves, allStandardMoves, opponentCount, opponentNum, allControllingFields
            , filterOutInCheck
            , move, move', updatePositionMove, moveToPieceMove, tryMoves
            , isMate
            , basicFen
            , fullFen
            , isChecking
            , filterOutInCheckFull, checkInRoute, isCheckInRoute, gameStateRoutes, routeData, returnCheckingRoute
            , inCheck
            , pieceFields
            , pieceFieldForMove
            , parseFen, fenToGameState, gameStateToFen
            , castlingRightsParser
            , allOpponentMoves
            , fenStringToPosition
            , canCastleKingSide, canCastleQueenSide
            ) where

import Data.Maybe
import Board
import Helpers
import Control.Lens
import Control.Monad
import Control.Applicative
import qualified Data.String.Utils as SU
import qualified Data.Char as C
import qualified Data.Text as Te
import qualified Text.Read as TR
import Data.Attoparsec.Text hiding (take, D, takeWhile)
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.Either.Combinators as EitherC
import Helpers
import Data.List
import System.IO.Unsafe

-- Defining fields that are checked often.
rowFields row = fmap (fromJust . stringToField) [c : (show row) | c <- ['A'..'H']]
[a1, b1, c1, d1, e1, f1, g1, h1] = rowFields 1 
[a8, b8, c8, d8, e8, f8, g8, h8] = rowFields 8

type CastlingRights = ((Bool, Bool), (Bool, Bool))

data GameState = GameState {
      _gsPosition :: Position
    , _gsColor :: Color
    , _castlingRights :: CastlingRights
    , _enPassantTarget :: Maybe Field
    , _halfMove :: Int
    , _fullMove :: Int} deriving (Eq, Show)
makeLenses ''GameState

defaultGameState :: Position -> Color -> GameState
defaultGameState ps color = GameState ps color ((True, True), (True, True)) Nothing 0 1

defaultGameStateNoCastle :: Position -> Color -> GameState
defaultGameStateNoCastle ps color = GameState ps color ((False, False), (False, False)) Nothing 0 1

movePiece :: GameState -> Move -> Piece
movePiece gs (Move from to _) = (selectByPosition (_gsPosition gs) [from] !! 0) ^. pfPiece

maybeMovePiece :: GameState -> Move -> Maybe Piece
maybeMovePiece gs (Move from to _) = fmap (view pfPiece) $ safeIndex 0 (selectByPosition (_gsPosition gs) [from])

-- This is a helper function that just updates the gamestate position, but not logic like
-- castling rights. It is vastly faster, and used to determine whether I'm moving into a check.
updatePositionMove :: GameState -> (Piece, Move) -> GameState
updatePositionMove gs (piece, mv@(Move from to promotion)) = GameState newPosition newColor cr ept hm fm
  where   newColor = invertColor oldColor
          oldColor = _gsColor gs
          oldPosition = _gsPosition gs
          removePosition = filter (\pf -> not (elem (pf ^. pfField) [from, to])) oldPosition
          newPosition = removePosition ++ [PieceField piece oldColor to]
          cr = _castlingRights gs
          ept = _enPassantTarget gs
          hm = _halfMove gs
          fm = _fullMove gs

moveToPieceMove :: GameState -> Move -> (Piece, Move)
moveToPieceMove gs mv = (movePiece gs mv, mv)

tryMoves :: Maybe GameState -> [Move] -> Maybe GameState
tryMoves Nothing _ = Nothing
tryMoves (Just gs) [] = Just gs
tryMoves (Just gs) (mv : rest) = if isLegalMove then tryMoves (Just (move gs pm)) rest else Nothing
  where pm = moveToPieceMove gs mv
        isLegalMove = elem mv $ fmap snd $ allNextLegalMoves gs


move :: GameState -> (Piece, Move) -> GameState
move gs (piece, mv@(Move from to promotion)) = GameState newPosition' newColor newCr newEp hm fm
  where   newColor = invertColor oldColor
          oldColor = _gsColor gs
          oldPosition = _gsPosition gs
          removeFields = [from, to] ++ epTarget
          removePosition = filter (\pf -> not (elem (pf ^. pfField) removeFields)) oldPosition
          Field colFrom rowFrom = from
          Field colTo rowTo = to
          isEP = isEnPassant ept piece (from, to)
          epTarget = if isEP then [Field colTo rowFrom] else []
          newPiece = if isJust promotion then fromJust promotion else piece
          newPosition = removePosition ++ [PieceField newPiece oldColor to]
          newPosition' = if isCastle 
            then (updateCastlingRook isCastleKing oldColor newPosition) 
            else newPosition
          isCastle = isCastling piece mv
          cr = _castlingRights gs
          ept = _enPassantTarget gs
          hm = _halfMove gs
          fm = _fullMove gs
          newCr = updateCastlingRights gs mv
          newEp = updateEnPassant gs mv
          isCastleKing = isCastle && (to == g1 || to == g8)


move' :: GameState -> Move -> GameState
move' gs mv = move gs $ moveToPieceMove gs mv

isCastling :: Piece -> Move -> Bool
isCastling King (Move from to mp) = (from == e1 && (to == g1 || to == c1)) || (from == e8 && (to == g8 || to == c8))
isCastling _ _ = False

type CastleKingSide = Bool

updatePosition :: Position -> Field -> Field -> Position
updatePosition ps field newField = newPf : [p | p <- ps, p /= pf]
    where pf = head $ filter (\p -> p ^. pfField == field) ps
          newPf = PieceField piece color newField
          piece = pf ^. pfPiece
          color = pf ^. pfColor

updateCastlingRook :: CastleKingSide -> Color -> Position -> Position
updateCastlingRook True White ps = updatePosition ps h1 f1
updateCastlingRook False White ps = updatePosition ps a1 d1
updateCastlingRook True Black ps = updatePosition ps h8 f8
updateCastlingRook False Black ps = updatePosition ps a8 d8


updateEnPassant :: GameState -> Move -> Maybe Field
updateEnPassant gs mv@(Move from to _)
  | pawnMovedTwo = beforeField 
  | otherwise = Nothing
  where
      pawnMovedTwo = movedPawn && distance == 2
      movedPawn = maybeMovePiece gs mv == Just Pawn
      distance = moveDistance (from, to)
      (Field fromC fromR) = from
      beforeRow = nextRow fromR (_gsColor gs)
      beforeField = liftM2 Field (Just fromC) beforeRow

nextRow :: Row -> Color -> Maybe Row
nextRow r White = intRow $ (rowInt r) + 1
nextRow r Black = intRow $ (rowInt r) - 1

updateCastlingRights :: GameState -> Move -> CastlingRights
updateCastlingRights gs mv = updatedBothRights where
    (castleWhite, castleBlack) = _castlingRights gs
    (castleK, castleQ) = if color == White then castleWhite else castleBlack
    updatedRights = (castleK && (not castleKLost), castleQ && (not castleQLost))
    updatedBothRights = if color == White then (updatedRights, castleBlack) else (castleWhite, updatedRights)
    color = _gsColor gs
    kingMoved = movePiece gs mv == King
    queenRookMoved = (color == White && mv ^. moveFrom == a1) || (color == Black && mv ^. moveFrom == a8)
    kingRookMoved = (color == White && mv ^. moveFrom == h1) || (color == Black && mv ^. moveFrom == h8)
    castleKLost = kingMoved || kingRookMoved
    castleQLost = kingMoved || queenRookMoved

pieceFieldForMove :: GameState -> Move -> PieceField
pieceFieldForMove gs mv = head [pf | pf <- _gsPosition gs, mv ^. moveFrom == pf ^. pfField]

allNextStates :: GameState -> [GameState]
allNextStates gs = fmap (move gs) (allNextLegalMoves gs)

isMate :: GameState -> Bool
isMate gs = noNextMoves && inCheck gs
    where noNextMoves = length (allNextLegalMoves gs) == 0

allOpponentMoves :: GameState -> [(Piece, Move)]
allOpponentMoves = allPhysicalMoves . invertGameStateColor

invertGameStateColor :: GameState -> GameState
invertGameStateColor (GameState position color cr ept hm fm) = GameState position (invertColor color) cr ept hm fm

allStandardMoves :: GameState -> [Move]
allStandardMoves gs = concat $ fmap (allStandardPhysicalMoves gs) (ownPieceFields gs)

allNextLegalMoves :: GameState -> [(Piece, Move)]
allNextLegalMoves gs = filterOutInCheck gs $ allPhysicalMoves gs
    
inCheck :: GameState -> Bool
inCheck gs@(GameState ps color cr ept hm fm) = kingPosition `elem` (fmap Just opponentFields)
  where opponentFields = fmap (view moveTo) $ allStandardMoves . invertGameStateColor $ gs
        kingPosition = listToMaybe $ getPositions gs King

isChecking :: GameState -> Bool
isChecking = inCheck . invertGameStateColor
            
notInCheck = not . inCheck

filterOutInCheckFull :: GameState -> [(Piece, Move)] -> [(Piece, Move)]
filterOutInCheckFull gs pms = filter notInCheck pms
    where notInCheck (piece, mv) = not $ isChecking $ updatePositionMove gs (piece, mv)

ownKingField gs = head $ fmap (view pfField) $ filter ((==King) . (view pfPiece)) $ ownPieceFields gs

filterOutInCheck :: GameState -> [(Piece, Move)] -> [(Piece, Move)]
filterOutInCheck gs moves = legalKingMoves ++ filteredOtherMoves
  where (kingMoves, otherMoves) = partition (\(p, m) -> p == King) moves
        color = _gsColor gs
        kingField = ownKingField gs
        allOpponentControlledFields = allControllingFields $ invertGameStateColor gs
        -- legalKingMoves = filterOutInCheckFull gs kingMoves
        filterControlling = \(_, m) -> not ((m ^. moveTo) `elem` allOpponentControlledFields)
        legalKingMoves = filter filterControlling kingMoves
        routes = gameStateRoutes gs
        routeFilter = \(_, m) -> not (isCheckInRoute gs m routes)
        filteredOtherMoves = filter routeFilter otherMoves
        -- filteredOtherMoves = filterOutInCheckFull gs otherMoves
        

-- filterLegalKingMoves :: GameState -> [Move] -> [Move]

-- isLegalKingMove :: [Move] -> [Field] -> Move -> Bool
-- isLegalKingMove opponentMoves opponentProtectedPieces mv = notChecked || takingUnprotected
--   where notChecked = not $ to `elem` $ fmap to opponentMoves
--         takingUn


-- All opponent pieces that could possibly check the king in the current position,
-- assuming a piece moves.
data CheckingRoute = CheckingRoute { routePiece :: PieceField, routeBetweenFields :: [Field], routeBetweenPiece :: Maybe PieceField, routeKingPosition :: Field} deriving Show

-- routeCheck :: CheckingRoute -> Bool 
-- routeCheck cr = not $ isJust $ routeBetweenPiece cr

checkInRoute :: CheckingRoute -> Move -> Bool
checkInRoute cr mv = (not inCheck && creatingCheck) || (inCheck && not preventingCheck)
  where creatingCheck = movingPieceInBetween && not pieceIsBetweenAfterMove && not takingPiece
        preventingCheck = inCheck && (pieceIsBetweenAfterMove || takingPiece)
        takingPiece = to == (routePiece cr) ^. pfField
        pieceIsBetweenAfterMove = to `elem` betweenFields
        to = mv ^. moveTo
        from = mv ^. moveFrom
        betweenFields = routeBetweenFields cr
        betweenPiece = routeBetweenPiece cr
        inCheck = not $ isJust $ routeBetweenPiece cr
        movingPieceInBetween = Just from == fmap (view pfField) betweenPiece
    

isCheckInRoute :: GameState -> Move -> [CheckingRoute] -> Bool
isCheckInRoute gs mv = any ((flip checkInRoute) mv)

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
returnCheckingRoute own opp kingField (isLine, fields) = cr
  where (before, after) = span (fmap not isOpponentPiece) $ zip fields (fmap accum fields)
        (beforeOpp, afterOpp) = (fmap fst before, fmap fst after)
        oppCount = length afterOpp
        firstOpp = head afterOpp
        accum = pieceAccumulator ownFields oppFields
        hasOpp = oppCount > 0 && firstOpp `elem` oppFields
        firstOppPieceField = head $ filter (\pf -> pf ^. pfField == firstOpp) opp
        ownFields = fmap (view pfField) own
        oppFields = fmap (view pfField) opp
        ownFieldsInList = dropWhile (\f -> not (f `elem` beforeOpp)) ownFields
        firstOwnPiece = safeIndex 0 ownFieldsInList -- Maybe Field
        ownPieceOnField f = safeIndex 0 $ filter (\pf -> pf ^. pfField == f) own -- Field -> Maybe PieceField
        firstOwnPieceField = join $ fmap ownPieceOnField firstOwnPiece -- Maybe PieceField
        oppPiece = firstOppPieceField ^.pfPiece
        oppPieceRightType = (isLine && oppPiece `elem` [Rook, Queen]) || (not isLine && oppPiece `elem` [Bishop, Queen])
        numberOwnFieldsInBefore = length $ filter (\pf -> pf ^. pfField `elem` beforeOpp) own
        correct = hasOpp && oppPieceRightType && (numberOwnFieldsInBefore <= 1)
        cr = if correct then Just (CheckingRoute firstOppPieceField beforeOpp firstOwnPieceField kingField) else Nothing

isOpponentPiece (_, (own, opp)) = opp

pieceAccumulator :: [Field] -> [Field] -> Field -> (Bool, Bool)
pieceAccumulator own opp f = (f `elem` own, f `elem` opp)


canAffectCheck kingField m = canReachKing kingField (m ^. moveTo) || canReachKing kingField (m ^. moveFrom)

canReachKing :: Field -> Field -> Bool
canReachKing f1 f2 = reachableByBishop f1 f2 || reachableByRook f1 f2

reachableByBishop :: Field -> Field -> Bool
reachableByBishop (Field c r) (Field c' r') = (ic - ir == ic' - ir') || (ir - ic == ic' - ir')
  where (ic, ir, ic', ir') = (columnInt c, rowInt r, columnInt c', rowInt r')

reachableByRook :: Field -> Field -> Bool
reachableByRook (Field c r) (Field c' r') = (c == c') || (r == r')

notCheckedOn :: GameState -> Field -> Bool
notCheckedOn gs f = not $ f `elem` opponentFields
  where opponentFields = fmap (view moveTo) $ allStandardMoves . invertGameStateColor $ gs

type MoveDirections = [Move]

allControllingFields :: GameState -> [Field]
allControllingFields gs = nonPawnFields ++ pawnFields
  where nonPawnFields = concat $ fmap (allControllingFieldsHelper gs) nonPawns
        (pawns, nonPawns) = partition (\pf -> pf ^. pfPiece == Pawn) $ ownPieceFields gs
        color = _gsColor gs
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
allControllingFieldsHelper gs@(GameState position color _ _ _ _) pf@(PieceField piece _ field) = fmap (view moveTo) goodMoves
    where fields = pieceFields pf
          withCount = fmap (opponentNum (fmap (view pfField) position)) fields
          goodFields = concat $ (fmap . fmap) fst $ fmap (takeWhile (\(_, c) -> c <= 1)) withCount
          goodMoveFields = [(from, to) | (from, to) <- zip (repeat field) goodFields] :: [MoveLocation]
          goodMoveFilterPawn = filterPawnMoves gs piece goodMoveFields
          goodMoves = concat $ fmap (addSpecialMoves piece color) goodMoveFilterPawn
          

allStandardPhysicalMoves :: GameState -> PieceField -> [Move]
allStandardPhysicalMoves gs@(GameState position color _ _ _ _) pf@(PieceField piece _ field) = goodMoves
    where fields = pieceFields pf
          withCount = fmap (opponentNum opponentFields) fields
          goodFields = concat $ fmap (takeWhile notOwn) $ (fmap . fmap) fst $ fmap (takeWhile (\(_, c) -> c <= 1)) withCount
          goodMoveFields = [(from, to) | (from, to) <- zip (repeat field) goodFields] :: [MoveLocation]
          goodMoveFilterPawn = filterPawnMoves gs piece goodMoveFields
          goodMoves = concat $ fmap (addSpecialMoves piece color) goodMoveFilterPawn
          notOwn f = not $ f `elem` (fmap (view pfField) (ownPieceFields gs))
          opponentFields = fmap (view pfField) $ ownPieceFields $ invertGameStateColor gs
          
opponentCount :: Eq a => [a] -> [a] -> Int -> [(a, Int)]
opponentCount fs [] n = zip fs (repeat n)
opponentCount (f:rest) opfs n = (f, nextNum) : opponentCount rest opfs nextNum
  where nextNum = if (f `elem` opfs ||  n >= 1) then n + 1 else n
opponentCount [] _ _ = []

opponentNum :: Eq a => [a] -> [a] -> [(a, Int)]
opponentNum f f' = opponentCount f' f 0


allPiecePhysicalMoves :: GameState -> PieceField -> [(Piece, Move)]
allPiecePhysicalMoves gs@(GameState position color _ _ _ _) pf@(PieceField piece _ field) = zip (repeat piece) (goodMoves ++ castMoves)
    where castMoves = if piece == King then fmap nonPawnMove $ possibleCastlingMoves gs else []
          goodMoves = allStandardPhysicalMoves gs pf


filterPawnMoves :: GameState -> Piece -> [MoveLocation] -> [MoveLocation]
filterPawnMoves gs Pawn ml = filter (isLegalPawnMove gs) ml
filterPawnMoves _ _ ml = ml

nonPawnMove :: MoveLocation -> Move
nonPawnMove (from, to) = Move from to Nothing

isLegalPawnMove :: GameState -> MoveLocation -> Bool
isLegalPawnMove gs ml@(from, to) = (isGoingForward && notTaking) || ((not isGoingForward) && (taking || isEP))
    where   isGoingForward = to ^. fieldColumn == from ^. fieldColumn
            taking = isTaking gs to
            notTaking = not taking
            isEP = isEnPassant (_enPassantTarget gs) Pawn (from, to)

isTaking :: GameState -> Field -> Bool
isTaking gs to = elem to $ fmap (view pfField) (opponentPieceFields gs)

freeForCastling gs = fmap (&&isNotChecked) castleFree
  where occ = fmap (view pfField) $ _gsPosition gs
        cont = allControllingFields $ invertGameStateColor gs
        kingField = head $ fmap (view pfField) $ filter (\pf -> pf ^. pfPiece == King) $ ownPieceFields gs
        isNotChecked = not $ kingField `elem` cont
        castleFree = [canCastleKingSide gs occ cont, canCastleQueenSide gs occ cont]

castlingMoves :: GameState -> [MoveLocation]
castlingMoves gs
  | _gsColor gs == White = [(e1, g1), (e1, c1)]
  | _gsColor gs == Black = [(e8, g8), (e8, c8)]
    
possibleCastlingMoves :: GameState -> [MoveLocation]
possibleCastlingMoves gs = catMaybes [useValueIfCondition m c | (m, c) <- zip (castlingMoves gs) (freeForCastling gs)]

useValueIfCondition :: a -> Bool -> Maybe a
useValueIfCondition a False = Nothing
useValueIfCondition a True = Just a

hasCastlingRight :: Color -> CastleKingSide -> CastlingRights -> Bool
hasCastlingRight White True ((x, _), _) = x
hasCastlingRight White False ((_, x), _) = x
hasCastlingRight Black True (_, (x, _)) = x
hasCastlingRight Black False (_, (_, x)) = x

noBadMove :: [Field] -> [Field] -> Bool
noBadMove badFields fields = not $ any (\f -> f `elem` badFields) fields

canCastleKingSide :: GameState -> [Field] -> [Field] -> Bool
canCastleKingSide (GameState _ White cr  _ _ _) occ cont = hasCastlingRight White True cr && noBadMove cont [f1, g1] && noBadMove occ [f1, g1]

canCastleKingSide (GameState _ Black cr  _ _ _) occ cont = hasCastlingRight Black True cr && noBadMove cont [f8, g8] && noBadMove occ [f8, g8]

canCastleQueenSide :: GameState -> [Field] -> [Field] -> Bool
canCastleQueenSide (GameState _ White cr  _ _ _) occ cont = hasCastlingRight White False cr && noBadMove cont [d1, c1]  && noBadMove occ [d1, c1, b1]
canCastleQueenSide (GameState _ Black cr  _ _ _) occ cont = hasCastlingRight Black False cr && noBadMove cont [d8, c8] && noBadMove occ [d8, c8, b8]

freeField :: GameState -> Field -> Bool
freeField gs f = not $ elem f $ fmap (view pfField) $ _gsPosition gs

castlingFree :: GameState -> [Field] -> Bool
castlingFree gs fs = and $ fmap (\f -> (notCheckedOn gs f) && (freeField gs f)) fs

-- Logic for the `move` function:
-- In the EP case, remove the pawn that's EP taken
isEnPassant :: Maybe Field -> Piece -> MoveLocation -> Bool
isEnPassant ept piece ml@(from, to) = piece == Pawn && ept == Just to

addSpecialMoves :: Piece -> Color -> MoveLocation -> [Move]
addSpecialMoves Pawn White ml@((Field _ R7), _) = addPromotionMoves ml
addSpecialMoves Pawn Black ml@((Field _ R2), _) = addPromotionMoves ml
addSpecialMoves _ _ ml@((from, to)) = [Move from to Nothing]

addPromotionMoves :: MoveLocation -> [Move]
addPromotionMoves (from, to) = [Move from to (Just piece) | piece <- allNonKingFullPieces]

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
            pieceLetter = (shortPiece piece) !! 0
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
epToString (Just f) = fmap C.toLower $ shortField f
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
castlingRightsParser s = ((wk, wq), (bk, bq))
  where
    wk = 'K' `elem` s
    bk = 'k' `elem` s
    wq = 'Q' `elem` s
    bq = 'q' `elem` s

castlingRightsToString :: CastlingRights -> String
castlingRightsToString ((wk, wq), (bk, bq)) = if length concatenated > 0 then concatenated else "-"
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
