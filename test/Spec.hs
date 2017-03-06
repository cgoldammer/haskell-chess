import Test.HUnit
import Lib
import qualified Data.Set as S


main :: IO ()
main = do
    runTestTT tests
    return ()

test1 = TestCase (assertEqual "fsdfsd" (1,3) (Lib.foo 2))


rookPosition = (A, R1)
possibleRookMoves = [(A, R2), (A, R7), (B, R1), (H, R1)]
impossibleRookMoves = [(A, R1), (B, R2)]
allRookMoves = rookMoves rookPosition

knightPosition = (A, R1)
possibleKnightMoves = [(B, R3), (C, R2)]
impossibleKnightMoves = [(A, R1), (B, R2)]
allKnightMoves = knightMoves knightPosition

bishopPosition = (B, R2)
possibleBishopMoves = [(A, R1), (C, R3), (C, R1), (H, R8)]
impossibleBishopMoves = [(B, R1), (B, R2)]
allBishopMoves = bishopMoves bishopPosition

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

allTests = possibleTests ++ mateTests

tests = TestList allTests

matePosition1 = [PieceField King White (A, R1)
              , PieceField Queen Black (A, R2)
              , PieceField King Black (A, R3)]

matePosition2 = [PieceField King White (A, R1)
              , PieceField Rook Black (A, R8)
              , PieceField Rook Black (B, R8)
              , PieceField King Black (C, R8)]

nonMatePosition2 = [PieceField King White (A, R1)
              , PieceField Rook Black (A, R8)
              , PieceField Rook Black (B, R8)
              , PieceField King Black (B, R4)]

mateTest gs = TestCase (assertBool mateMessage (isMate mateState))
    where mateMessage = "This should be mate: " ++ (show mateState)
          mateState = GameState gs White

nonMateTest gs = TestCase (assertBool mateMessage (not (isMate mateState)))
    where mateMessage = "This should not be mate: " ++ (show mateState)
          mateState = GameState gs White


mateTests = fmap mateTest [matePosition1, matePosition2] ++ fmap nonMateTest [nonMatePosition2]
