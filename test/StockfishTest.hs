module StockfishTest (stockfishTests) where

import Test.HUnit
import qualified Data.Set as S
import Data.Attoparsec.Text hiding (take, D, takeWhile)
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.Text as Te
import Control.Monad
import Data.Maybe
import Data.Either
import qualified Data.Either.Combinators as EitherC
import Data.Either (rights)
import Data.List
import Control.Applicative
import qualified Turtle as Tu

import Chess.Algorithms
import Chess.Board
import Chess.Logic
import Chess.Stockfish 
import Chess.Pgn.Logic as Pgn


positionsMate = [
  (["WKG6", "WPG7", "WNH4", "BKG8", "BPA7"], "H4F5", 2)
  ]

-- Stockfish finds the unique mate in 2 in a position
testFindMate (psList, mvExpected, numExpected) = TestCase $ do
  let fen = fullFen . fromJust . stringToPosition $ psList
  let gs = fromJust $ fenToGameState fen
  let (_, moveExpected) = fromJust . (stringToMove gs) $ mvExpected
  mateMoves <- mateFinder fen
  length mateMoves > 0 @? "There are no mates, but there should be!"
  let (mv, num) = mateMoves !! 0
  mv @?= moveExpected
  num @?= numExpected
  
positionsBest = [
  (["WKG1", "WPG2", "WNF1", "BKG8", "BQE3"], "F1E3", 300, 10000)
  ]

-- The evaluation for the positions is in the expected range.
testFindBest (psList, mvExpected, numExpectedLower, numExpectedHigher) = TestCase $ do
  let fen = fullFen . fromJust . stringToPosition $ psList
  let gs = fromJust $ fenToGameState fen
  let (_, moveExpected) = fromJust . (stringToMove gs) $ mvExpected
  mvs <- bestMoves fen 1000 1 1
  let sfm@(StockfishMove mv _ _ eval) = mvs !! 0
  mv @?= moveExpected
  sortMove sfm > numExpectedLower @? "Expected evaluation bigger than: " ++ show numExpectedLower ++ show sfm
  sortMove sfm < numExpectedHigher @? "Expected evaluation smaller than: " ++ show numExpectedHigher ++ show sfm

findMateTests = fmap testFindMate positionsMate
findBestTests = fmap testFindBest positionsBest

moveSummaryTest = TestCase $ do
  let moves = ["e4", "e5", "Qh5", "Ke7", "Qg5+"]
  let game = head $ rights [gameFromStart pgnToMove moves]
  summaries <- gameSummaries 50 game
  assertEqual "A summary exists for each move" (length summaries) (length moves)
  let lastSummary = last summaries
  assertEqual "At the end, the best move is the mate" "Qxe5#" (msMoveBest lastSummary) 

stockfishTests = TestList [
    "find mate tests" ~: findMateTests
  , "find best tests" ~: findBestTests
  , "the move summary works correctly" ~: moveSummaryTest
  ]
