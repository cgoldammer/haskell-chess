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

intersect :: Ord a => [a] -> [a] -> S.Set a
intersect l1 l2 = S.intersection (S.fromList l1) (S.fromList l2)

testPossible :: (Ord a, Show a) => String -> [a] -> [a] -> [a] -> [Test]
testPossible name ps imps all = [testPossible, testImpossible]
  where testPossible = TestCase (assertBool (name ++ " possible moves: " ++ (show intersectionPossible)) conditionPossible)
        testImpossible = TestCase (assertBool (name ++ " impossible moves") conditionImpossible)
        conditionPossible = length intersectionPossible == length ps
        conditionImpossible = length intersectionImpossible == 0
        intersectionPossible = intersect ps all
        intersectionImpossible = intersect imps all


knightTests = testPossible "knight" possibleKnightMoves impossibleKnightMoves allKnightMoves
rookTests = testPossible "rook" possibleRookMoves impossibleRookMoves allRookMoves
possibleTests = knightTests ++ rookTests

tests = TestList possibleTests





