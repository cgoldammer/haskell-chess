module LogicTest (logicTests) where

import Control.Lens hiding ((.=))
import Test.HUnit
import qualified Data.Set as S
import qualified Data.Text as Te
import Data.Maybe
import qualified Data.Either.Combinators as EitherC

import Chess.Algorithms
import Chess.Board
import Chess.Logic

rookPosition = Field A R1
possibleRookMoves = ["A2", "A7", "B1", "H1"]
impossibleRookMoves = ["A1", "B2"]
allRookMoves = concat $ pieceFields $ PieceField Rook White rookPosition

knightPosition = Field A R1
possibleKnightMoves = ["B3", "C2"]
impossibleKnightMoves = ["A1", "B2"]
allKnightMoves = concat $ pieceFields $ PieceField Knight White knightPosition

bishopPosition = Field B R2
possibleBishopMoves = ["A1", "C3", "C1", "H8"]
impossibleBishopMoves = ["B1", "B2"]
allBishopMoves = concat $ pieceFields $ PieceField Bishop White bishopPosition

intersect :: Ord a => [a] -> [a] -> S.Set a
intersect l1 l2 = S.intersection (S.fromList l1) (S.fromList l2)

testFromString :: String -> [String] -> [String] -> [Field] -> [Test]
testFromString name possible impossible all = testPossible name posPossible posImpossible all
    where [posPossible, posImpossible] = fmap (\strings -> (catMaybes (fmap stringToField strings))) [possible, impossible]
                
testPossible :: (Ord a, Show a) => String -> [a] -> [a] -> [a] -> [Test]
testPossible name possible impossible all = [testPossible, testImpossible]
  where testPossible = TestCase (assertBool possibleMessage conditionPossible)
        testImpossible = TestCase (assertBool impossibleMessage conditionImpossible)
        conditionPossible = length intersectionPossible == length possible
        conditionImpossible = length intersectionImpossible == 0
        [intersectionPossible, intersectionImpossible] = fmap (\v -> intersect v all) [possible, impossible]
        possibleMessage = name ++ " possible moves: " ++ (show possible)
        impossibleMessage = name ++ " impossible moves: " ++ (show intersectionPossible)

knightTests = testFromString "knight" possibleKnightMoves impossibleKnightMoves allKnightMoves
rookTests = testFromString "rook" possibleRookMoves impossibleRookMoves allRookMoves
bishopTests = testFromString "bishop" possibleBishopMoves impossibleBishopMoves allBishopMoves
possibleTests = knightTests ++ rookTests ++ bishopTests

matePositions = [["WKA1", "BQA2", "BKA3"],
                 ["WKA1", "BRA8", "BRB8", "BKC8"]]

nonMatePositions = [["WKA1", "BRA8", "BRB8", "BKB4"]]

mateTest gs = TestCase (assertBool mateMessage (isMate gs))
    where mateMessage = "This should be mate, but isn't: " ++ (show gs)

nonMateTest gs = TestCase (assertBool mateMessage (not (isMate gs)))
    where mateMessage = "This should not be mate, but is: " ++ (show gs)

toGameState :: Position -> GameState
toGameState ps = defaultGameState ps White

toGameStateNoCastle :: Position -> GameState
toGameStateNoCastle ps = defaultGameStateNoCastle ps White

mates = fmap toGameStateNoCastle $ catMaybes (fmap stringToPosition matePositions)
nonMates = fmap toGameStateNoCastle $ catMaybes (fmap stringToPosition nonMatePositions)

allNextFields = []
movesTest expected ps = TestCase (assertBool errorMessage (movesCalculated == movesExpected))
    where   movesCalculated = S.fromList $ allLegalMoves ps
            movesExpected = S.fromList expected
            errorMessage = "Position: " ++ show ps ++ " Expected: " ++ show movesExpected ++ " but got: " ++ show movesCalculated

movesTests = fmap (\m -> ("No moves in " ++ show m) ~: movesTest allNextFields m) mates
mateTests = fmap mateTest mates ++ fmap nonMateTest nonMates


conditionHolds :: (GameState -> Bool) -> String -> GameState -> Test
conditionHolds fn name gs = TestCase (assertBool (name ++ ": " ++ show gs) (fn gs))

checkTests = fmap (conditionHolds inCheck "Should be in check, but isn't") (mates ++ nonMates)
isCheckTests = fmap (conditionHolds (not . isChecking) "Should not be checking, but is") (mates ++ nonMates)

oppMoves :: [Field]
oppMoves = fmap ((view moveTo) . snd) $ allOpponentMoves $ mates !! 0
oppMoveTest = TestCase $ assertBool (show oppMoves) ((fromJust (stringToField "A1")) `elem` oppMoves)

pawnPosition = ["WKA1", "WPE4", "WPD2", "WPC2", "BKA8", "BPD4", "BPD5"]
pawnP = catMaybes (fmap stringToPieceField pawnPosition)

whiteGS = defaultGameState pawnP White
pawnTestWhite = testFromString "white pawns" ["C3", "C4", "D3", "E5", "D5"] [] $ pawnToFields whiteGS

blackGS = invertGameStateColor whiteGS
pawnTestBlack = testFromString "black pawns" ["E4", "D3"] ["E3"] $ pawnToFields blackGS

blackGSEP = GameState pawnP Black castleNone (Just (Field E R3)) 0 1
pawnTestBlackEP = testFromString "black pawns" ["E4", "D3", "E3"] [] $ pawnToFields blackGSEP

promotePos = catMaybes $ fmap stringToPieceField ["WKA1", "WPE7", "BKA8", "BRF8", "WKE8"]
promoteGS = defaultGameState promotePos White

expectedPawnMoves = [PromotionMove (Field E R7) (Field F R8) piece | piece <- allNonKingFullPieces]
allPawnMoves = pawnToMoves promoteGS
pawnTestPromote = testPossible "promotion" expectedPawnMoves [] allPawnMoves

pawnToFields :: GameState -> [Field]
pawnToFields gs = fmap (view moveTo) $ pawnToMoves gs

pawnToMoves :: GameState -> [Move]
pawnToMoves gs = [mv | (_, mv) <- allLegalMoves gs, (mv ^. moveFrom) `elem` (getPositions gs Pawn)]

pawnTests = TestList [
    "white" ~: pawnTestWhite
  , "black" ~: pawnTestBlack
  , "en passant" ~: pawnTestBlackEP
  , "promote" ~: pawnTestPromote]


-- all castling combinations should be possible, thus depend on the gamestate castling rights.
posCastleBoth = fromJust $ stringToPosition ["WRA1", "WKE1", "WRH1", "BKG8"]
gsCastleBoth = toGameState posCastleBoth

testCastleBoth = TestCase $ assertEqual error (S.fromList movesExpected) intersection
    where error = "Both castling moves should be possible for white"
          intersection = intersect possible movesExpected
          possible = allLegalMoves gsCastleBoth
          movesExpected = catMaybes $ fmap (stringToMove gsCastleBoth) ["E1C1", "E1G1"]

posCastleBothBlack = fromJust $ stringToPosition ["WKE1", "BKE8", "BKA8", "BRH8"]
gsCastleBothBlack = invertGameStateColor $ toGameState posCastleBothBlack

testCastleBothBlack = TestCase $ assertEqual error (S.fromList movesExpected) intersection
    where error = "Both castling moves should be possible for black"
          intersection = intersect possible movesExpected
          possible = allLegalMoves gsCastleBothBlack
          movesExpected = catMaybes $ fmap (stringToMove gsCastleBothBlack) ["E8C8", "E8G8"]


-- castleDestroyedData = [("A1D1", CastlingData True False), ("H1H2", CastlingData False True)]
-- testCastleDestroyed move expectedState = TestCase $ assertEqual error expectedState actualState
--   where error = "Moving a rook should destroy castling rights on that side"
--         gsNew = 
    


-- after white castles kingside (queenside), the rook is on f1 (c1)
testRookField mv expectedField = TestCase $ assertEqual error (S.fromList expectedFields) intersection
    where error = "Rook expected on: "
          expectedFields = [fromJust (stringToField expectedField)]
          intersection = intersect fields expectedFields
          gsAfterCastle = uncurry (move gsCastleBoth) $ fromJust (stringToMove gsCastleBoth mv)
          fields = fmap (view pfField) $ gsAfterCastle ^. gsPosition

testRookFields = [
      testRookField "E1G1" "F1"
    , testRookField "E1C1" "C1"]

gsWithCastling :: Position -> Color -> CastlingRights -> GameState
gsWithCastling ps color cr = GameState ps color cr Nothing 0 1

testCastleSide cr mv = TestCase $ assertEqual error (S.fromList movesExpected) intersection
    where error = "Exactly one castle should be possible" ++ show gsCastleOne ++ " | All moves: " ++ show kingFields
          movesExpected = fmap snd $ catMaybes $ fmap (stringToMove gsCastleOne) [mv]
          gsCastleOne = gsWithCastling posCastleBoth White cr
          possible = fmap snd $ allLegalMoves gsCastleOne
          intersection = intersect possible movesExpected
          kingFields = fmap ((view moveTo) . snd) $ filter (\m -> fst m == King) $ allLegalMoves gsCastleOne

crKing = PlayerData (CastlingData True False) (CastlingData False False)
crQueen = PlayerData (CastlingData False True) (CastlingData False False)
testCastleOneSide = [
    testCastleSide crKing "E1G1",
    testCastleSide crQueen "E1C1"]

-- Queenside castling is allowed, kingside castling is not (because of rook on f8)
posCastleQueen = fromJust $ stringToPosition ["WRA1", "WKE1", "WRH1", "BKG8", "BRF8"]
gsCastleQueen = toGameState posCastleBoth
testCastleQueen = TestCase $ assertEqual error (S.fromList movesExpected) intersection
    where error = "Only queenside castle should be possible"
          intersection = intersect possible movesExpected
          possible = fmap snd $ allLegalMoves gsCastleQueen
          movesExpected = fmap snd $ catMaybes $ fmap (stringToMove gsCastleQueen) ["E1C1"]

testLosesRight mvs mvExpected = TestCase $ assertEqual error movesExpected difference
  where error = "Check that some castling rights are lost after moving: " ++ show mvs
        movesExpected = S.fromList $ catMaybes $ fmap (stringToMove gsCastleBoth) mvExpected
        difference = S.difference movesBefore movesAfter
        movesAfter = S.fromList $ allLegalMoves newState
        movesBefore = S.fromList $ allLegalMoves gsCastleBoth
        newState = newStateFromMoves gsCastleBoth mvs

testsLosesRight = [
    testLosesRight ["E1F1", "G8F8", "F1E1", "F8G8"] ["E1G1", "E1C1"]
  , testLosesRight ["A1A2", "G8F8", "A2A1", "F8G8"] ["E1C1"]
  , testLosesRight ["H1H2", "G8F8", "H2H1", "F8G8"] ["E1G1"]]
    
-- After a promotion, the initial pawn disappears
stringToGs = toGameStateNoCastle . fromJust . stringToPosition

-- White to move can take ep iff the gamestate has an ep pawn on e5.
-- If it's black's move, white can take ep on c6 (and only c6) iff black plays c5.
gsEp = stringToGs ["WKA1", "BKA8", "WPC2", "WPE2", "BPD4"]

testEp mvs mvExpected error = TestCase $ assertEqual error (S.fromList movesExpected) intersection
  where movesExpected = catMaybes $ fmap (stringToMove newState) mvExpected -- [Move]
        intersection = intersect possible movesExpected
        possible = filter (\mp -> fst mp == Pawn) $ allLegalMoves newState
        newState = newStateFromMoves gsEp mvs

testsEp = [
    testEp ["C2C4"] ["D4D3", "D4C3"] "Can take the c-pawn en passant"
  , testEp ["E2E4"] ["D4D3", "D4E3"] "Can take the d-pawn en passant"
  , testEp ["E2E4", "A8B8", "A1B1"] ["D4D3"] "Cannot take the c-pawn after intermediate moves"]

ownPieces :: GameState -> Piece -> [Field]
ownPieces gs p = fmap (view pfField) $ filter ((==p) . (view pfPiece)) $ ownPieceFields gs

newStateFromMoves :: GameState -> [String] -> GameState
newStateFromMoves gs mvs = last $ gameStates $ fromJust $ EitherC.rightToMaybe game
  where game = gameFromString stringToMove gs mvs

fieldsFromString s = fmap (fromJust . stringToField) s

testEpDisappears = TestCase $ assertEqual error intersection (S.fromList whiteFields)
  where intersection = intersect whiteFields fieldsExpected
        newState = newStateFromMoves gsEp ["C2C4", "D4C3"]
        whiteFields = ownPieces newState Pawn
        fieldsExpected = fieldsFromString ["E2"]
        error = "Pawn that is taken ep disappears - incorrect GameState: " ++ show newState

gsPromote = stringToGs ["WKA1", "BKA8", "WPG7"]

testPromotionPawnDisappears = TestCase $ assertEqual error (S.fromList fieldsExpected) intersection
  where intersection = intersect whiteFields fieldsExpected
        newState = invertGameStateColor $ newStateFromMoves gsPromote ["G7G8N"]
        whiteFields = ownPieces newState Pawn
        fieldsExpected = []
        error = "Pawn that promotes disappears" ++ show newState

testPromotionNewPiece = TestCase $ assertEqual error (S.fromList fieldsExpected) intersection
  where intersection = intersect whiteFields fieldsExpected
        newState = invertGameStateColor $ newStateFromMoves gsPromote ["G7G8N"]
        whiteFields = ownPieces newState Knight
        fieldsExpected = fieldsFromString ["G8"]
        error = "New knight has appeared: " ++ show newState

-- You can promote by taking.
gsPromoteTake = stringToGs ["WKA1", "BKA8", "WPG7", "BNH8"]

testPromotionTake = TestCase $ assertEqual error (S.fromList fieldsExpected) intersection
  where intersection = intersect whiteFields fieldsExpected
        newState = newStateFromMoves gsPromoteTake ["G7H8N"]
        whiteFields = ownPieces newState Knight
        fieldsExpected = fieldsFromString []
        error = "The knight should be taken: " ++ show newState

legalMoveData = [
    (["WKA1", "BKA8"], ["A1A2", "A1B1", "A1B2"], ["A1B3"])
  , (["WKA1", "BKA8", "BRB7"], ["A1A2"], ["A1B2"])
  , (["WKA1", "BKA8", "WQA2", "BRA7"], ["A2A3"], ["A1A2", "A2B2", "A2A8"])
  , (["WKA1", "BKA8", "WQB2", "BRA7"], ["B2A3"], ["B2B3", "B2A1"])
  , (["WKA1", "BKA8", "BRA7", "BBH8"], ["A1B1"], ["A1B2", "A1B2"])
  , (["WKA1", "BKA8", "BBE3", "WPD2", "WPE2"], ["D2E3"], ["E2E3", "E2E4"])
  , (["WKA1", "BKA8", "BRA7", "WPA3", "BPB4"], ["A3A4"], ["A3B4"])]

legalMoveTest gsString legalMoves = (S.fromList expected) ~=? actual
  where actual = intersect expected legal
        gs = stringToGs gsString
        expected = fmap (snd . fromJust . (stringToMove gs)) legalMoves
        legal = fmap snd $ allLegalMoves gs


illegalMoveTest gsString illegalMoves = expected ~=? actual
  where expected = fmap (snd . fromJust . (stringToMove gs)) illegalMoves
        gs = stringToGs gsString
        legalMoves = fmap snd $ allLegalMoves gs
        actual = filter (\m -> not (m `elem` legalMoves)) expected


fensToConvert = [
    "fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  , "fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1"
  , "fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b kq - 0 1"
  , "fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b - - 20 1"
  , "fen rnbqkbnr/ppp2ppp/4p3/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 1 1"]

-- If I take a fen, convert it to a gamestate, and then back to a fen, I should get the
-- initial fen back.
fenDoubleConvertTest f = Just f ~=? fmap gameStateToFen (fenToGameState f)

fenDoubleConvertTests = TestList $ fmap fenDoubleConvertTest fensToConvert

legalMoveTests = ["Test legal moves are right" ~: legalMoveTest f s | (f, s, _) <- legalMoveData]
illegalMoveTests = ["Test illegal moves are right" ~: illegalMoveTest f t | (f, _, t) <- legalMoveData]

singleTests = [
      "Opponent Move test" ~: oppMoveTest
    , "Pawn that is taken en passant disappears" ~: testEpDisappears
    , "Castling Queen" ~: testCastleQueen
    , "Castling Both black" ~: testCastleBothBlack
    , "Pawn that promotes disappears" ~: testPromotionPawnDisappears
    , "Promoting pawn creates piece" ~: testPromotionNewPiece
    , "Promoting can happen by taking" ~: testPromotionTake
    , "Castling Both" ~: testCastleBoth]

logicTests = TestList [
    "Check tests" ~: checkTests
  , "is check tests" ~: isCheckTests
  , "mate tests" ~: mateTests
  , "pawn tests" ~: pawnTests
  , "single tests" ~: singleTests
  , "castle one side" ~: testCastleOneSide
  , "rook fields" ~: testRookFields
  , "losing castling rights" ~: testsLosesRight
  , "En passant" ~: testsEp
  , "Legal moves" ~: legalMoveTests
  , "illegal moves" ~: illegalMoveTests
  , "moves" ~: movesTests
  , "possible" ~: possibleTests
  , "fen double conversion" ~: fenDoubleConvertTests
  ]

