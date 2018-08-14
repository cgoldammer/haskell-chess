module Chess.Logic (allPhysicalMoves, allPieceMoves
            , GameState (..), gsPosition, gsColor, gsCastlingRights, gsEnPassantTarget
            , CastlingRights (..), PlayerData (..), CastlingData (..)
            , castleNone, castleAll
            , defaultGameState, defaultGameStateNoCastle
            , startingGS
            , getPositions
            , invertGameStateColor
            , ownPieceFields
            , allLegalMoves, opponentCount, opponentNum, allControllingFields
            , filterOutInCheck
            , parsePromotionPiece
            , move, move', updatePositionMove, tryMoves, movePiece
            , isMate, ownKingField
            , isChecking, isTaking, isLegalPawnMove
            , filterOutInCheckFull, checkInRoute, gameStateRoutes, routeData, returnCheckingRoute
            , inCheck, removeKing
            , pieceFields
            , pieceFieldForMove
            , parseFen, fenToGameState, gameStateToFen, fullFen
            , castlingRightsParser, freeForCastling
            , allOpponentMoves
            , fenStringToPosition
            , stringToMove
            , a1, b1, c1, d1, e1, f1, g1, h1, a8, b8, c8, d8, e8, f8, g8, h8
            , MoveReader
            , Game (..)
            , gameFromString, gameFromStart
            ) where


import Debug.Trace (trace)
import Data.Maybe (listToMaybe, fromJust, Maybe(..), catMaybes, isJust, isNothing)
import Control.Lens (makeLenses, view, (^.), over, each, both)
import Control.Monad (liftM2, liftM4, join)
import Control.Applicative (liftA2, (<|>), optional)
import Data.String.Utils (replace)
import Data.Char (toLower, toUpper)
import Data.Text (pack)
import Data.Attoparsec.Text (Parser, parseOnly, string, letter, digit, char, space, decimal)
import Data.Attoparsec.Combinator (many1', endOfInput)
import Data.Either.Combinators (rightToMaybe)
import Data.List (partition, intercalate)
import Data.Foldable (fold, foldMap)

import Chess.Types
import Chess.Board
import Chess.Helpers
import Chess.Fen

-- The following fields are often used as part of the chess logic, so I'm defining
-- them as variables and exporting them.
rowFields row = fmap (fromJust . stringToField) [c : show row | c <- ['A'..'H']]
[a1, b1, c1, d1, e1, f1, g1, h1] = rowFields 1 
[a8, b8, c8, d8, e8, f8, g8, h8] = rowFields 8

getPlayerData :: PlayerData a -> Color -> a
getPlayerData playerData White = dataWhite playerData
getPlayerData playerData Black = dataBlack playerData

getCastlingData :: CastlingData a -> CastleKingSide -> a
getCastlingData castlingData True = canKingSide castlingData
getCastlingData castlingData False = canQueenSide castlingData

canCastle :: CastlingData Bool -> Bool
canCastle (CastlingData False False) = False
canCastle _ = True

-- |Creating default game states in which players can either castle or not castle
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

startingGS :: GameState
startingGS = defaultGameState (fmap (fromJust . stringToPieceField) startGameString) White


-- |Given a gamestate and a move, return which piece made the move
movePiece :: GameState -> Move -> Piece
movePiece gs mv = head matchingPieces ^. pfPiece
  where from = mv ^. moveFrom
        matchingPieces = selectByPosition (gs ^. gsPosition) [from]
        
-- |Given a Maybe GameState, try to make a list of moves.
-- This returns Nothing whenever a move isn't legal or when the starting state 
-- was Nothing. This function can be used to safely fold over a list of moves to get 
-- the final state.
tryMoves :: Maybe GameState -> [Move] -> Maybe GameState
tryMoves Nothing _ = Nothing
tryMoves (Just gs) [] = Just gs
tryMoves (Just gs) (mv : rest) = if isLegalMove then nextState else Nothing
  where piece = movePiece gs mv
        isLegalMove = elem mv $ snd <$> allLegalMoves gs
        nextState = tryMoves (Just (move gs piece mv)) rest 

type IsTaking = Bool
type IsPawnMove = Bool

-- |The full move increases by one after black has made a move.
updateFullMove :: Color -> Int -> Int
updateFullMove White n = n
updateFullMove Black n = n + 1

-- |The half move updates whenever the gamestate changes irrevokably. This is
-- true for all non-standard moves. For standard moves, this is true if it's a pawn
-- move or a piece is taken.
updateHalfMove :: GameState -> Move -> Int -> Int
updateHalfMove _ EnPassantMove{} _ = 1
updateHalfMove _ PromotionMove{} _ = 1
updateHalfMove _ CastlingMove{}  _ = 1
updateHalfMove gs mv@(StandardMove from to) n = if structureChanged then 1 else updateFullMove color n
  where isTakingMove = isTaking gs to
        isPawnMove = movePiece gs mv == Pawn
        color = gs ^. gsColor
        structureChanged = isTakingMove || isPawnMove


-- |The main logic required to update a position. Give a gamestate, and a piece
-- making a move, this returns a tuple containing a list of fields that are now
-- vacated and a list of PieceFields with changed piece positions.
-- This does not involve removing taken pieces, that's done through 
-- `getPositionChange`.
getPositionChangeHelper :: GameState -> Piece -> Move -> ([Field], [PieceField])
getPositionChangeHelper gs piece (StandardMove from to) = (removePf, addPf)
  where position = gs ^. gsPosition
        removePf = [from]
        color = gs ^. gsColor
        addPf = [PieceField piece color to]
getPositionChangeHelper gs Pawn (PromotionMove promotionFrom promotionTo promotionPiece) = (removePf, addPf)
  where position = gs ^. gsPosition
        removePf = [promotionFrom]
        color = gs ^. gsColor
        addPf = [PieceField promotionPiece color promotionTo]
getPositionChangeHelper gs King (CastlingMove from to rookFrom rookTo) = (removePf, addPf)
  where position = gs ^. gsPosition
        removePf = [from, rookFrom]
        color = gs ^. gsColor
        addPf = [PieceField King color to, PieceField Rook color rookTo]
getPositionChangeHelper gs Pawn (EnPassantMove from to pawnCapturedField) = (removePf, addPf)
  where position = gs ^. gsPosition
        color = gs ^. gsColor
        removePf = [from, pawnCapturedField]
        addPf = [PieceField Pawn color to]
getPositionChangeHelper _ _ _ = ([], [])

-- |The interface to update a position. This has almost the same as
-- getPositionChangeHelper, except that pieces that are taken are removed.
getPositionChange :: GameState -> Piece -> Move -> ([Field], [PieceField])
getPositionChange gs piece move = (removePf ++ removeTaken, addPf)
  where toField = move ^. moveTo
        isTakingMove = isTaking gs toField
        (removePf, addPf) = getPositionChangeHelper gs piece move
        removeTaken = [toField | isTakingMove]

-- |A helper function to shift a field into a direction that depends on the player's color.
shiftFieldIntoOwnDirection :: Field -> Color -> Int -> Maybe Field
shiftFieldIntoOwnDirection (Field column row) color number = Field <$> Just column <*> takenRow
  where rowNumber = rowInt row
        rowChange = number * (-1) * rowChangeForColor color
        takenRow = intRow $ rowNumber + rowChange

-- |Remove a certain field from the position
removeFieldFromPosition :: Position -> Field -> Position
removeFieldFromPosition position field = [pf | pf <- position, pf ^. pfField /= field]

-- |Update a position given the fields of pieces that are to be removed and the piecefields
-- to add.
updatePosition :: Position -> [Field] -> [PieceField] -> Position
updatePosition position remove add = foldl removeFieldFromPosition position remove ++ add

moveData :: GameState -> Piece -> Move -> (Color, Color, Position, Position)
moveData gs piece mv = (oldColor, newColor, oldPosition, newPosition)
  where   oldColor = gs ^. gsColor
          newColor = invertColor oldColor
          oldPosition = gs ^. gsPosition
          newPosition = uncurry (updatePosition oldPosition) $ getPositionChange gs piece mv

-- |The main logic to update a `GameState` based on a move made by a piece.
-- Logically, it's not necessary to include the piece (since it can be inferred from the move)
-- but the piece is crucial for updating the position, and would have to be looked up, so passing
-- it along can speed up performance.
-- Conceptually, the function updates all components of the `GameState` using helper functions
-- which are usually called `update...`, e.g. `updateHalfMove`.
move :: GameState -> Piece -> Move -> GameState
move gs piece mv = GameState newPosition newColor newCastlingRights newEnPassant newHalfMove newFullMove
  where   (oldColor, newColor, oldPosition, newPosition) = moveData gs piece mv
          newHalfMove = updateHalfMove gs mv (gs ^. gsHalfMove)
          newFullMove = updateFullMove oldColor (gs ^. gsFullMove)
          newCastlingRights = updateCastling oldColor mv (gs ^. gsCastlingRights)
          newEnPassant = updateEnPassant gs mv

updateCastling :: Color -> Move -> CastlingRights -> CastlingRights
updateCastling White mv (PlayerData w b) = PlayerData (updateCastlingRights White mv w) b
updateCastling Black mv (PlayerData w b) = PlayerData w (updateCastlingRights Black mv b)

-- |Based on a `GameState`, a piece and a move, return a new `GameState`.
-- Conceptually, this is the central function for this library, because it encapsulates
-- the full logic of making moves and updating a `GameState`.
-- Even though this is the most parsimonious signature, for performance reasons 
-- the function is defined in terms of `move`, which also takes the piece 
-- corresponding to the move.
move' :: GameState -> Move -> GameState
move' gs mv = move gs (movePiece gs mv) mv

-- | This is a helper function that just updates the gamestate position, but not logic like
-- castling rights. It is vastly faster, and used to determine whether I'm moving into a check.
updatePositionMove :: GameState -> Piece -> Move -> GameState
updatePositionMove gs piece mv = GameState newPosition newColor oldCastlingRights oldEp oldHalfMove oldFullMove
  where   (oldColor, newColor, oldPosition, newPosition) = moveData gs piece mv
          oldCastlingRights = gs ^. gsCastlingRights
          oldHalfMove = gs ^. gsHalfMove
          oldFullMove = gs ^. gsFullMove
          oldEp = gs ^. gsEnPassantTarget


-- | Obtaining an en passant target given a move. The logic is that
-- A new en-passant target exists if and only if a pawn moved two squares.
-- In that case, the target is one row before where the pawn ends up, e.g.
-- After White plays e2-e4, the en-passant target is on e3.
updateEnPassant :: GameState -> Move -> Maybe Field
updateEnPassant gs mv@(StandardMove from@(Field fromC fromR) to@(Field toC toR))
  | pawnMovedTwo = beforeField 
  | otherwise = Nothing
  where
      pawnMovedTwo = movedPawn && distance == 2
      movedPawn = movePiece gs mv == Pawn
      distance = moveDistance (from, to)
      beforeRow = nextRow fromR $ gs ^. gsColor
      beforeField = liftM2 Field (Just fromC) beforeRow
updateEnPassant gs _ = Nothing

rowChangeForColor :: Color -> Int
rowChangeForColor White = 1
rowChangeForColor Black = -1

nextRow :: Row -> Color -> Maybe Row
nextRow r color = intRow $ rowInt r + rowChangeForColor color

-- |Update castling rights given the existing rights and flags for whether the current 
-- move left the state the same. E.g. if I castle myself, then the flag would be False.
updatePlayerCastlingData :: CastlingData Bool -> CastlingData Bool -> CastlingData Bool
updatePlayerCastlingData (CastlingData kBefore qBefore) (CastlingData kNow qNow) = CastlingData (kBefore && kNow) (qBefore && qNow)

updateCastlingRights :: Color -> Move -> CastlingData Bool -> CastlingData Bool
updateCastlingRights _ _ (CastlingData False False) = CastlingData False False
updateCastlingRights _ (CastlingMove from to rookFrom rookTo) _ = CastlingData False False
updateCastlingRights White (StandardMove fromField _) (CastlingData k q)
  | fromField == a1 = CastlingData k False
  | fromField == e1 = CastlingData False False
  | fromField == h1 = CastlingData False q
updateCastlingRights Black (StandardMove fromField _) (CastlingData k q)
  | fromField == a8 = CastlingData k False
  | fromField == e8 = CastlingData False False
  | fromField == h8 = CastlingData False q
updateCastlingRights _ _ castlingRights = castlingRights
  
pieceFieldForMove :: GameState -> Move -> PieceField
pieceFieldForMove gs mv = head [pf | pf <- gs ^. gsPosition, mv ^. moveFrom == pf ^. pfField]

invertGameStateColor :: GameState -> GameState
invertGameStateColor = over gsColor invertColor


-- | `allLegalMoves` is the main function here. We obtain legal moves as follows:
-- 1. Obtain all own pieces. For each piece, obtain the standard moves (excluding promotion and castling)
-- 2. Add promotion and castling moves if appropriate for the piece.
-- 3. Concatenate all resulting moves. These are the `allPhysicalMoves`.
-- 4. Filter out all moves that are illegal because they would leave the king in check.
--    This is done with `filterOutInCheck`. That function is complicated, because running this
--    check naively is extremely inefficient, see the documentation of the function for details.
allLegalMoves :: GameState -> [(Piece, Move)]
allLegalMoves gs = filterOutInCheck gs $ allPhysicalMoves gs

allPhysicalMoves :: GameState -> [(Piece, Move)]
allPhysicalMoves gs = concatMap (allPieceMoves True gs) (ownPieceFields gs)

allNextStates :: GameState -> [GameState]
allNextStates gs = fmap (uncurry (move gs)) (allLegalMoves gs)

isMate :: GameState -> Bool
isMate gs = noNextMoves && inCheck gs
  where noNextMoves = null $ allLegalMoves gs

allOpponentMoves :: GameState -> [(Piece, Move)]
allOpponentMoves = allPhysicalMoves . invertGameStateColor
    
inCheck :: GameState -> Bool
inCheck gs@(GameState ps color cr ept hm fm) = ownKingField gs `elem` opponentFields
  where opponentFields = fmap (view moveTo . snd) $ allStandardMoves . invertGameStateColor $ gs
        allStandardMoves gs = concatMap (allPieceMoves False gs) (ownPieceFields gs)

removeKing :: GameState -> GameState
removeKing gs@(GameState ps color cr ept hm fm) = GameState psNew color cr ept hm fm
  where psNew = updatePosition ps kingFields []
        kingFields = fmap _pfField $ filter (\pf -> _pfPiece pf == King && _pfColor pf == color) ps

isChecking :: GameState -> Bool
isChecking = inCheck . invertGameStateColor
            
filterOutInCheckFull :: GameState -> [(Piece, Move)] -> [(Piece, Move)]
filterOutInCheckFull gs = filter notInCheck 
    where notInCheck (piece, mv) = not $ inCheck $ updatePositionMove gs piece mv

ownKingField gs = head $ fmap (view pfField) $ filter ((==King) . view pfPiece) $ ownPieceFields gs

-- Given a `GameState`, this function removes all physically possible
-- moves that would be illegal because they would leave the own king in check.
-- Warning: This function is tricky, and it'd be much less code to simply use
-- `isChecking` on the resulting `GameState`. However, doing so would be vastly
-- slower.
-- Here's why: In a given position, isChecking looks at all possible moves, and 
-- sees whether the king is in them. Let's say that there are n possible moves.
-- Then, to find all legal moves, we will need to calculate n^2 new gameStates
-- (n for the initial move, and another n for each of them to see whether that
-- GameState results in a check).
-- In contrast, this method will have complexity of n+m, where m is the 
-- number of possible ways in which a move can result in a check from the 
-- starting position.
-- The method works as follows:
-- (1) What are the squares that the king can go to that are not controlled
-- by opponent pieces? Moving to these squares is a legal move.
-- (2) If it's a piece move:
--   a) If the king is in check: Does the piece move interrupt the check?
--   b) If the king is not in check: Is the piece pinned (moving the piece would create a check)?
-- In both those cases, the move is illegal and filtered out.
filterOutInCheck :: GameState -> [(Piece, Move)] -> [(Piece, Move)]
filterOutInCheck gs moves = legalKingMoves ++ filteredOtherMoves
  where (kingMoves, pieceMoves) = partition (\(p, m) -> p == King) moves
        kingField = ownKingField gs
        allOpponentControlledFields = allControllingFields $ invertGameStateColor $ removeKing gs
        filterControlling (_, m) = (m ^. moveTo) `notElem` allOpponentControlledFields
        legalKingMoves = filter filterControlling kingMoves
        routes = gameStateRoutes gs
        routeFilter (_, m) = not ((fmap any checkInRoute) m routes)
        filteredOtherMoves = filter routeFilter pieceMoves

-- In our approach, we want to find a small sufficient statistic that helps
-- us determine whether the change in the position leads to a check. This is
-- the `CheckingRoute`.
data CheckingRoute = CheckingRoute { 
  routePiece :: PieceField
, routeBetweenFields :: [Field]
, routeBetweenPiece :: Maybe PieceField
, routeKingPosition :: Field} deriving Show

checkInRoute :: Move -> CheckingRoute -> Bool
checkInRoute mv (CheckingRoute piece betweenFields betweenPiece _) = (not inCheck && creatingCheck) || (inCheck && not preventingCheck)
  where creatingCheck = movingPieceInBetween && not pieceIsBetweenAfterMove && not takingPiece
        preventingCheck = inCheck && (pieceIsBetweenAfterMove || takingPiece)
        takingPiece = to == piece ^. pfField
        pieceIsBetweenAfterMove = to `elem` betweenFields
        (to, from) = (mv ^. moveTo, mv ^. moveFrom)
        inCheck = isNothing betweenPiece
        movingPieceInBetween = Just from == fmap (view pfField) betweenPiece
    
type IsLine = Bool

routeData :: GameState -> ([PieceField], [PieceField], Field, [(IsLine, [Field])])
routeData gs = (own, opp, kingField, allMoves)
  where kingField = ownKingField gs
        pieceData = [(Rook, True), (Bishop, False)]
        pieceMoves = [[(isLine, pf) | pf <- pieceFields (PieceField piece White kingField)] | (piece, isLine) <- pieceData]
        allMoves = filter (not . null . snd) $ concat pieceMoves
        (own, opp) = (ownPieceFields gs, opponentPieceFields gs)

gameStateRoutes :: GameState -> [CheckingRoute]
gameStateRoutes gs = catMaybes $ fmap (returnCheckingRoute own opp kingField) allMoves
  where (own, opp, kingField, allMoves) = routeData gs

-- Returns the checking routes. To do this, you need the king position, the 
-- position of all own and oppoent fields, and a route (a list of fields).
-- Then, we go through the fields and start counting both own and opponent pieces
-- on those fields. If the first two pieces encountered are own pieces, then
-- it's not a checking route. If there is no opponent piece in the route, then it's 
-- also not a checking route.
returnCheckingRoute :: [PieceField] -> [PieceField] -> Field -> (IsLine, [Field]) -> Maybe CheckingRoute
returnCheckingRoute _ _ _ (_, []) = Nothing
returnCheckingRoute own opp kingField (isLine, fields) = liftM4 CheckingRoute opponentPiece fieldsBefore (Just firstOwnPieceField) (Just kingField)
  where before = span (fmap not isOpponentPiece) $ zip fields pieceOwners
        (beforeOpp, afterOpp) = (over both) (fmap fst) before
        opponentPiece = routeOpponentPiece isLine opp (listToMaybe afterOpp)
        (ownFields, oppFields) = (over both) (fmap (view pfField)) (own, opp)
        fieldsBefore = routeBeforeFields own beforeOpp
        pieceOwners = fmap (pieceOwner ownFields oppFields) fields
        ownFieldsInList = dropWhile (`notElem`beforeOpp) ownFields
        ownPieceOnField f = listToMaybe $ filter ((==f) . view pfField) own
        firstOwnPiece = listToMaybe ownFieldsInList
        firstOwnPieceField = join $ fmap ownPieceOnField firstOwnPiece

-- A helper to determine whether the condition for number of own pieces (at most one)
-- is satisfied
routeBeforeFields :: [PieceField] -> [Field] -> Maybe [Field]
routeBeforeFields own beforeOpp = makeMaybe (numberOwnFieldsInBefore <= 1) beforeOpp
  where numberOwnFieldsInBefore = length $ filter (\pf -> pf ^. pfField `elem` beforeOpp) own

-- A helper to determined whether the condition for opponent pieces is satisfied:
-- There must be an opponent piece, and it must be of the right type (e.g. if 
-- we are searching over a line it must be a rook or queen).
routeOpponentPiece :: IsLine -> [PieceField] -> Maybe Field -> Maybe PieceField
routeOpponentPiece _ _ Nothing = Nothing
routeOpponentPiece isLine opponentFields (Just opponentField) = makeMaybe oppPieceRightType firstOppPieceField
  where 
        oppPiece = firstOppPieceField ^.pfPiece
        firstOppPieceField = head $ filter (\pf -> pf ^. pfField == opponentField) opponentFields
        oppPieceRightType = (isLine && oppPiece `elem` [Rook, Queen]) || (not isLine && oppPiece `elem` [Bishop, Queen])

isOpponentPiece (_, (own, opp)) = opp

-- Given a set of own fields and opponent fields, tells us whether
-- the field is in either set.
pieceOwner :: [Field] -> [Field] -> Field -> (Bool, Bool)
pieceOwner own opp f = (f `elem` own, f `elem` opp)

type MoveDirections = [Move]

allControllingFields :: GameState -> [Field]
allControllingFields gs = nonPawnFields ++ pawnFields
  where nonPawnFields = concatMap (allControllingFieldsHelper gs) nonPawns
        (pawns, nonPawns) = partition ((==Pawn) . view pfPiece) $ ownPieceFields gs
        color = gs ^. gsColor
        pawnFields = concatMap (pawnTakingFields color . view pfField) pawns

-- The fields that a pawn on a given square can move to if taking.
pawnTakingFields :: Color -> Field -> [Field]
pawnTakingFields color (Field col row) = catMaybes $ fmap (\col -> fmap (Field col) newRow) newCols
  where colNum = columnInt col
        newRow = nextRow row color
        newCols = catMaybes $ fmap (\d -> intColumn (colNum + d)) [-1, 1]

allControllingFieldsHelper :: GameState -> PieceField -> [Field]
allControllingFieldsHelper gs@(GameState position color _ _ _ _) pf@(PieceField piece _ field) = fmap snd $ goodMoveFilterPawn
    where fields = pieceFields pf
          withCount = fmap (opponentNum (fmap (view pfField) position)) fields
          goodFields = concat $ fmap fst <$> fmap (takeWhile ((<=1) . snd)) withCount
          goodMoveFields = [(from, to) | (from, to) <- zip (repeat field) goodFields] :: [MoveLocation]
          goodMoveFilterPawn = filterPawnMoves gs piece goodMoveFields
          
allOpponentFields gs = fmap (view pfField) $ ownPieceFields $ invertGameStateColor gs

doesPawnPromote :: Piece -> Field -> Color -> Bool
doesPawnPromote Pawn field color = (field ^. fieldRow) == promotionRow
  where promotionRow = if color == White then R7 else R2
doesPawnPromote _ _ _ = False

type IsAddingSpecialMoves = Bool

-- | Returns all moves that a piece can make given a `GameState`.
-- This includes the `addSpecialMoves` flag that toggles whether we add special moves
-- (castling and pawn promotions). This flag is used for performance reasons, as calculating
-- these special moves is time-intensive, and there are situations where we want
-- to know the next moves that pieces can make but we do not need to include the special moves.
allPieceMoves :: IsAddingSpecialMoves -> GameState -> PieceField -> [(Piece, Move)]
allPieceMoves addSpecialMoves gs pf@(PieceField piece _ field) = [(piece, mv) | mv <- allMoves]
  where opponentFields = allOpponentFields gs
        goodFields = allPieceTargets gs pf opponentFields
        goodMoveFields = [(from, to) | (from, to) <- zip (repeat field) goodFields] :: [MoveLocation]
        completeGoodFields = filterPawnMoves gs piece goodMoveFields
        allMoves = addSpecialtyMoves addSpecialMoves gs pf completeGoodFields

-- | This provides the possible destination fields for a piece. This works as follows:
-- 1. Obtain a list of list that provides all possible destinations if the board was empty.
--    It's a list of list because its contains all moves for each possible direction.
-- 2. Scan through these moves, and stop if
--   a. You find an own piece on a destination field or 
--   b. You have encountered a second opponent piece
--   Any move before that is physically reachable, thus this function filters out anything
--   before (a) or (b) happen.
allPieceTargets :: GameState -> PieceField -> [Field] -> [Field]
allPieceTargets gs pf opponentFields = goodFields
  where withCount = fmap (opponentNum opponentFields) fieldsIfBoardEmpty
        fieldsIfBoardEmpty = pieceFields pf
        notOwn f = f `notElem` fmap (view pfField) (ownPieceFields gs)
        goodFields = concatMap (takeWhile notOwn) $ fmap fst <$> fmap (takeWhile (\(_, c) -> c <= 1)) withCount

addSpecialtyMoves :: IsAddingSpecialMoves -> GameState -> PieceField -> [MoveLocation] -> [Move]
addSpecialtyMoves true gs (PieceField piece _ field) fields = movesWithPromotion ++ castMoves
  where movesWithPromotion = concatMap (turnFieldsToMove pawnPromotes gs piece) fields
        pawnPromotes = doesPawnPromote piece field (gs ^. gsColor)
        castMoves = if piece == King then possibleCastlingMoves gs else []
addSpecialtyMoves false gs (PieceField piece _ _) fields = concatMap (turnFieldsToMove false gs piece) fields
    

moveFromFields :: GameState -> Piece -> (Field, Field) -> Move
moveFromFields gs piece (from, to) = if isEp then EnPassantMove from to captureField else StandardMove from to
  where isEp = isEnPassant (_gsEnPassantTarget gs) piece (from, to)
        captureField = fromJust $ shiftFieldIntoOwnDirection to (gs ^. gsColor) 1

type IsPromotion = Bool

turnFieldsToMove :: IsPromotion -> GameState -> Piece -> MoveLocation -> [Move]
turnFieldsToMove True gs piece (from, to) = [PromotionMove from to piece | piece <- allNonKingFullPieces]
turnFieldsToMove False gs piece (from, to) = [moveFromFields gs piece (from, to)]

opponentCount :: Eq a => [a] -> [a] -> Int -> [(a, Int)]
opponentCount fs [] n = zip fs (repeat n)
opponentCount (f:rest) opfs n = (f, nextNum) : opponentCount rest opfs nextNum
  where nextNum = if f `elem` opfs || n >= 1 then n + 1 else n
opponentCount [] _ _ = []

opponentNum :: Eq a => [a] -> [a] -> [(a, Int)]
opponentNum f f' = opponentCount f' f 0

filterPawnMoves :: GameState -> Piece -> [MoveLocation] -> [MoveLocation]
filterPawnMoves gs Pawn ml = filter (isLegalPawnMove gs) ml
filterPawnMoves _ _ ml = ml

isLegalPawnMove :: GameState -> MoveLocation -> Bool
isLegalPawnMove gs ml@(from, to) = (isGoingForward && notTaking) || (not isGoingForward && (taking || isEP))
    where   isGoingForward = to ^. fieldColumn == from ^. fieldColumn
            taking = isTaking gs to
            notTaking = not taking
            isEP = isEnPassant (_gsEnPassantTarget gs) Pawn (from, to)

isTaking :: GameState -> Field -> Bool
isTaking gs to = elem to $ fmap (view pfField) (opponentPieceFields gs)

freeForCastling gs = if inCheck gs then [] else [kingCurrent && kingFields, queenCurrent && queenFields]
  where occupied = view pfField <$> _gsPosition gs
        controlled = allControllingFields $ invertGameStateColor gs
        color = gs ^. gsColor
        castlingFieldsForPlayer = getPlayerData castlingFields color
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

type OccupiedField = Field
type ControlledByOpponentField = Field
type ShouldNotBeOccupiedField = Field
type ShouldNotBeControlledField = Field

getCastleOption :: ([OccupiedField], [ControlledByOpponentField]) -> ([ShouldNotBeOccupiedField], [ShouldNotBeControlledField]) -> Bool
getCastleOption (occupied, controlled) (mustNotBeOccupied, mustNotBeControlled) = noBadMove occupied mustNotBeOccupied && noBadMove controlled mustNotBeControlled

noBadMove :: [Field] -> [Field] -> Bool
noBadMove badFields fields = not $ any (`elem`badFields) fields

freeField :: GameState -> Field -> Bool
freeField gs f = notElem f $ view pfField <$> _gsPosition gs

isEnPassant :: Maybe Field -> Piece -> MoveLocation -> Bool
isEnPassant ept piece (from, to) = piece == Pawn && ept == Just to

ownPieceFields :: GameState -> [PieceField]
ownPieceFields gs = filter (\pf -> pf ^. pfColor == _gsColor gs) (_gsPosition gs)

opponentPieceFields :: GameState -> [PieceField]
opponentPieceFields = ownPieceFields . invertGameStateColor 

fieldStep :: Field -> (Int, Int) -> Maybe Field
fieldStep (Field c r) (x, y) = makeMaybe allLegit $ Field newCol newRow
    where   cInt = columnInt c
            rInt = rowInt r
            (newX, newY) = (cInt + x, rInt + y)
            (newCol, newRow) = (fromJust (intColumn newX), fromJust (intRow newY))
            allLegit = isJust (intColumn newX) && isJust (intRow newY)

boardRange :: Bool -> [Int]
boardRange True = [1..7]
boardRange False = reverse [(-7)..(-1)]

boardRangeT = boardRange True
boardRangeF = boardRange False

rookSteps = [zip boardRangeF (repeat 0), zip boardRangeT (repeat 0), zip (repeat 0) boardRangeF, zip (repeat 0) boardRangeT]
knightSteps = (:[]) <$> [(sc, sr) | sc <- [-2, -1, 1, 2], sr <- [-2, -1, 1, 2], abs sc + abs sr == 3]
bishopSteps = [zip boardRangeF boardRangeT, zip boardRangeT boardRangeT, zip boardRangeF boardRangeF, zip boardRangeT boardRangeF]
queenSteps = rookSteps ++ bishopSteps
kingSteps = (:[]) <$> [(sc, sr) | sc <- [-1..1], sr <- [-1..1], abs sc + abs sr > 0]

type StepMove = (Int, Int)
movesFromSteps :: [[StepMove]] -> Field -> [[Field]]
movesFromSteps steps sf = fmap (catMaybes . fmap (fieldStep sf)) steps

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
getPositions gs pc = view pfField <$> filter isRightPiece position
    where   isRightPiece (PieceField pfP pfC field) = (pfP == pc) && (pfC == color)
            color = _gsColor gs
            position = _gsPosition gs
            
selectByPosition :: Position -> [Field] -> Position
selectByPosition ps fs = filter (\pf -> (pf ^. pfField) `elem` fs) ps

piecesOnField :: Position -> Field -> Maybe PieceField
piecesOnField ps f = safeIndex 0 (filter byField ps)
    where   byField pf = pf ^. pfField == f

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
  | length p == 1 && head p `elem` ("QRBN" :: String) = stringToPiece p
  | otherwise = Nothing

nonCastleStringToMove :: GameState -> String -> Maybe (Piece, Move)
nonCastleStringToMove gs (c1 : c2 : c3 : c4 : rest) = (,) <$> fromPiece <*> move
  where from = stringToField [c1, c2]
        to = stringToField [c3, c4] 
        color = gs ^. gsColor
        promotionPiece = parsePromotionPiece rest
        fromPiece = listToMaybe [pf ^. pfPiece | pf <- gs ^. gsPosition, Just (pf ^. pfField) == from]
        epTarget = gs ^. gsEnPassantTarget
        isEp = epTarget == to && fromPiece == Just Pawn
        pawnCapturedField = join $ fmap (\field -> shiftFieldIntoOwnDirection field color 1) epTarget
        moveEnPassant = EnPassantMove <$> from <*> to <*> pawnCapturedField
        movePromotion = PromotionMove <$> from <*> to <*> promotionPiece
        moveStandard = StandardMove <$> from <*> to
        move = if not isEp then if isJust promotionPiece then movePromotion else moveStandard else moveEnPassant

type MoveReader = GameState -> String -> Maybe (Piece, Move)

type FullState = (Maybe GameState, Maybe (Piece, Move))

pgnMoveFolder :: MoveReader -> FullState -> String -> FullState
pgnMoveFolder mr (Nothing, _) _ = (Nothing, Nothing)
pgnMoveFolder mr (Just gs, _) pgnMove = (gs', mv)
  where mv = mr gs pgnMove
        gs' = fmap (uncurry (move gs)) mv

gsFromStringMoves :: MoveReader -> GameState -> [String] -> [FullState]
gsFromStringMoves mr gs = scanl (pgnMoveFolder mr) (Just gs, Nothing)

gsFromStringMovesStart :: MoveReader -> [String] -> [FullState]
gsFromStringMovesStart mr = gsFromStringMoves mr startingGS

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
          Just (gs, m) -> gameFromStringAccum mr (Right (gs : currentGs : restGs, m : mvList)) restStringMoves
          Nothing         -> Left error
  where gameMoveNumber = div (length mvList) 2
        error = "Parsed moves until " ++ show gameMoveNumber ++ show (reverse mvList)
        parsedMove = tryParsingMove mr currentGs nextStringMove -- Maybe (GameState, Piece, Move)

tryParsingMove :: MoveReader -> GameState -> String -> Maybe (GameState, Move)
tryParsingMove mr gs stringMove = (,) <$> executedMove <*> fmap snd parsedMove
  where parsedMove = mr gs stringMove -- Maybe (Piece, Move)
        executedMove = fmap (uncurry (move gs)) parsedMove -- Maybe GameState
    
gameFromStart :: MoveReader -> [String] -> Either String Game
gameFromStart mr = gameFromString mr startingGS

