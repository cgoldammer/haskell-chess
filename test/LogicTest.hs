module LogicTest (logicTests) where

import Test.HUnit
import Algorithms
import Board
import Logic
import Various
import qualified Data.Set as S
import Data.Maybe




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
testFromString name ps imps all = testPossible name psS impsS all
    where   psS = catMaybes $ fmap stringToField ps
            impsS = catMaybes $ fmap stringToField imps
                

testPossible :: (Ord a, Show a) => String -> [a] -> [a] -> [a] -> [Test]
testPossible name ps imps all = [testPossible, testImpossible]
  where testPossible = TestCase (assertBool possibleMessage conditionPossible)
        testImpossible = TestCase (assertBool impossibleMessage conditionImpossible)
        conditionPossible = length intersectionPossible == length ps
        conditionImpossible = length intersectionImpossible == 0
        intersectionPossible = intersect ps all
        intersectionImpossible = intersect imps all
        possibleMessage = name ++ " possible moves: " ++ (show all)
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

toGameState ps = defaultGameState ps White
mates = fmap toGameState $ catMaybes (fmap stringToPosition matePositions)
nonMates = fmap toGameState $ catMaybes (fmap stringToPosition nonMatePositions)

allNextFields = []

movesTest expected ps = TestCase (assertBool errorMessage (movesCalculated == movesExpected))
    where   movesCalculated = S.fromList $ allNextLegalMoves ps
            movesExpected = S.fromList expected
            errorMessage = "Position: " ++ show ps ++ " Expected: " ++ show movesExpected ++ " but got: " ++ show movesCalculated

conditionHolds :: (GameState -> Bool) -> String -> GameState -> Test
conditionHolds fn name gs = TestCase (assertBool (name ++ ": " ++ show gs) (fn gs))

checkTests = fmap (conditionHolds inCheck "Should be in check, but isn't") (mates ++ nonMates)
isCheckTests = fmap (conditionHolds (not . isChecking) "Should not be checking, but is") (mates ++ nonMates)

oppMoves :: [Field]
oppMoves = fmap (moveTo . snd) $ allOpponentMoves $ mates !! 0
oppMoveTest = TestCase $ assertBool (show oppMoves) ((fromJust (stringToField "A1")) `elem` oppMoves)

pawnPosition = ["WKA1", "WPE4", "WPD2", "WPC2", "BKA8", "BPD4", "BPD5"]
pawnP = catMaybes (fmap stringToPieceField pawnPosition)

whiteGS = defaultGameState pawnP White
pawnTestWhite = testFromString "white pawns" ["C3", "C4", "D3", "E5", "D5"] [] $ pawnToFields whiteGS

blackGS = invertGameStateColor whiteGS
pawnTestBlack = testFromString "black pawns" ["E4", "D3"] ["E3"] $ pawnToFields blackGS

blackGSEP = GameState pawnP Black ((False, False), (False, False)) (Just (Field E R3)) 0 1
pawnTestBlackEP = testFromString "black pawns" ["E4", "D3", "E3"] [] $ pawnToFields blackGSEP

promotePos = catMaybes $ fmap stringToPieceField ["WKA1", "WPE7", "BKA8", "BRF8", "WKE8"]
promoteGS = defaultGameState promotePos White

expectedPawnMoves = [Move (Field E R7) (Field F R8) (Just piece) | piece <- allNonKingFullPieces]
allPawnMoves = pawnToMoves promoteGS
pawnTestPromote = testPossible "promotion" expectedPawnMoves [] allPawnMoves

pawnToFields :: GameState -> [Field]
pawnToFields gs = fmap moveTo $ pawnToMoves gs

pawnToMoves :: GameState -> [Move]
pawnToMoves gs = allPawnToFields
    where   allPawnFields = getPositions gs Pawn
            allPawnToFields = [mv | (_, mv) <- allNextLegalMoves gs, (moveFrom mv) `elem` allPawnFields]
pawnTests = pawnTestWhite ++ pawnTestBlack ++ pawnTestBlackEP ++ pawnTestPromote

movesTests = fmap (movesTest allNextFields) mates
mateTests = fmap mateTest mates ++ fmap nonMateTest nonMates
allTests = possibleTests ++ movesTests ++ [oppMoveTest] ++ checkTests ++ isCheckTests ++ mateTests ++ pawnTests
logicTests = allTests
