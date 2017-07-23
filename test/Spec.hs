import Test.HUnit
import Algorithms
import Board
import Logic
import Various
import qualified Data.Set as S
import Data.Maybe
import LogicTest
import PgnTest
import StringParseTest

main :: IO ()
main = do
    runTestTT tests
    return ()

tests = TestList $ logicTests ++ stringParseTests ++ pgnTests
