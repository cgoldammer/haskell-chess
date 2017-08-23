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
import StockfishTest

main :: IO ()
main = do
    runTestTT tests
    -- runTestTT stockfishTests
    return ()

tests = TestList [
    "Logic tests: " ~: logicTests
  , "String parse tests: " ~: stringParseTests
  , "Pgn game parsing tests:" ~: pgnTests
  -- , "Stockfish tests" ~: stockfishTests
  ]
