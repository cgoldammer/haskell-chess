import Test.HUnit
import qualified Data.Set as S
import Data.Maybe

import Chess.Algorithms
import Chess.Board
import Chess.Logic

import LogicTest
import StringParseTest
import PgnTest
import StockfishTest

main :: IO ()
main = runTestTT tests >> return ()

tests = TestList [
    "Logic tests: " ~: logicTests
  , "String parse tests: " ~: stringParseTests
  , "Pgn tests:" ~: pgnTests
  , "Stockfish tests" ~: stockfishTests
  ]
