module StringParseTest (stringParseTests) where

import Test.HUnit

import Data.Set
import Data.Maybe

import Chess.Algorithms
import Chess.Board
import Chess.Logic

moveStrings = [
      ("E2E4", Just (StandardMove (Field E R2) (Field E R4)), ["WKE1", "WPE2", "BKE8"], defaultGameStateNoCastle, White)
    , ("E7E8", Just (StandardMove (Field E R7) (Field E R8)), ["WKE1", "WRE7", "BKC8"], defaultGameStateNoCastle, White)
    , ("E7E8N", Just (PromotionMove (Field E R7) (Field E R8) Knight), ["WKE1", "BKA8", "WPE7"], defaultGameStateNoCastle, White)
    , ("E2E9", Nothing, ["WKE1", "WRE2", "BKC8"], defaultGameStateNoCastle, White)
    , ("E1G1", Just (CastlingMove e1 g1 h1 f1), ["WKE1", "WRH1", "BKC8"], defaultGameState, White)
    , ("A1I2", Nothing, ["WKB1", "WQA1", "BKC8"], defaultGameStateNoCastle, White)]

testMoveString ms expected gs = TestCase $ assertEqual errorString expected parsed
    where errorString = "From" ++ ms ++ ": Expected " ++ show expected ++ " but got " ++ show parsed
          parsed = fmap snd $ stringToMove gs ms -- Maybe Move

stringParseTests = fmap testCreator moveStrings
  where testCreator (ms, expected, positionList, gameStateFunction, color) = testMoveString ms expected $ gameStateFunction (catMaybes $ fmap stringToPieceField positionList) color


