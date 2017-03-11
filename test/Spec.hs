import Test.HUnit
import Lib
import qualified Data.Set as S
import Data.Maybe

main :: IO ()
main = do
    runTestTT tests
    return ()

rookPosition = (A, R1)
possibleRookMoves = [(A, R2), (A, R7), (B, R1), (H, R1)]
impossibleRookMoves = [(A, R1), (B, R2)]
allRookMoves = allPieceFields Rook rookPosition

knightPosition = (A, R1)
possibleKnightMoves = [(B, R3), (C, R2)]
impossibleKnightMoves = [(A, R1), (B, R2)]
allKnightMoves = allPieceFields Knight knightPosition

bishopPosition = (B, R2)
possibleBishopMoves = [(A, R1), (C, R3), (C, R1), (H, R8)]
impossibleBishopMoves = [(B, R1), (B, R2)]
allBishopMoves = allPieceFields Bishop bishopPosition

intersect :: Ord a => [a] -> [a] -> S.Set a
intersect l1 l2 = S.intersection (S.fromList l1) (S.fromList l2)

testPossible :: (Ord a, Show a) => String -> [a] -> [a] -> [a] -> [Test]
testPossible name ps imps all = [testPossible, testImpossible]
  where testPossible = TestCase (assertBool possibleMessage conditionPossible)
        testImpossible = TestCase (assertBool impossibleMessage conditionImpossible)
        conditionPossible = length intersectionPossible == length ps
        conditionImpossible = length intersectionImpossible == 0
        intersectionPossible = intersect ps all
        intersectionImpossible = intersect imps all
        possibleMessage = name ++ " possible moves: " ++ (show intersectionPossible)
        impossibleMessage = name ++ " impossible moves: " ++ (show intersectionPossible)

knightTests = testPossible "knight" possibleKnightMoves impossibleKnightMoves allKnightMoves
rookTests = testPossible "rook" possibleRookMoves impossibleRookMoves allRookMoves
bishopTests = testPossible "bishop" possibleBishopMoves impossibleBishopMoves allBishopMoves
possibleTests = knightTests ++ rookTests ++ bishopTests


matePositions = [["WKA1", "BQA2", "BKA3"],
                 ["WKA1", "BRA8", "BRB8", "BKC8"]]


nonMatePositions = [["WKA1", "BRA8", "BRB8", "BKB4"]]

mateTest gs = TestCase (assertBool mateMessage (isMate gs))
    where mateMessage = "This should be mate, but isn't: " ++ (show gs)

nonMateTest gs = TestCase (assertBool mateMessage (not (isMate gs)))
    where mateMessage = "This should not be mate, but is: " ++ (show gs)

toGameState ps = GameState ps White
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

oppMoves = fmap snd $ allOpponentMoves $ mates !! 0
oppMoveTest = TestCase $ assertBool (show oppMoves) ((fromJust (stringToField "A1")) `elem` oppMoves)

movesTests = fmap (movesTest allNextFields) mates
mateTests = fmap mateTest mates ++ fmap nonMateTest nonMates
allTests = possibleTests ++ movesTests ++ [oppMoveTest] ++ checkTests ++ isCheckTests ++ mateTests
tests = TestList allTests
