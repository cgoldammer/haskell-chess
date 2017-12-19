import Test.HUnit
import qualified Data.Set as S
import Data.Maybe

import LogicTest
import PgnTest
import StringParseTest
import StockfishTest

import Chess.Algorithms
import Chess.Board
import Chess.Logic

main :: IO ()
main = do
    runTestTT tests
    return ()

tests = TestList [
    "Logic tests: " ~: logicTests
  -- , "String parse tests: " ~: stringParseTests
  -- , "Pgn game parsing tests:" ~: pgnTests
  -- , "Stockfish tests" ~: stockfishTests
  ]
