{-# LANGUAGE ScopedTypeVariables #-}
module PgnTest (pgnTests) where

import Test.HUnit
import Algorithms
import Board
import Logic
import Various 
import qualified Data.Set as S
import Data.Attoparsec.Text hiding (take, D, takeWhile)
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.ByteString.Char8 as C
import Pgn
import qualified Data.Text as Te
import Control.Monad
import Data.Maybe
import Data.Either
import Control.Applicative
import qualified Turtle as Tu

-- I can parse a string, from a starting position
moveString = ["e4", "e5", "Ne2", "Nc6", "N2c3"]
expected = ["E2E4", "E7E5", "G1E2", "B8C6", "E2C3"]

gs :: Either String GameState
gs = parseOnly parseFen (Te.pack startGameFen)
testStartingFen = TestCase $ assertBool ("Starting game state not parsed" ++ show gs) (isRight gs)

parseFirst :: Parser String = do
    positionFen :: String <- many (letter <|> digit <|> (char '/'))
    return positionFen

pos = fenStringToPosition . catMaybes . sequence . maybeResult . (parse parseFirst) . Te.pack $ startGameFen
testPositionParse = TestCase $ assertBool ("Starting position parsed with : " ++ (show (length pos)) ++ show pos) (length pos == 32)

startingGS = fromJust $ either (const Nothing) Just gs

fullMoves = [
      ("E2E4", Pawn, "e4")
    , ("G1F3", Knight, "Nf3")]

moveTest :: String -> Piece -> String -> Test
moveTest moveString piece pgnString = TestCase $ assertEqual "not equal simple pgn test" pgnString pgnMoveParse
    where   pgnMoveParse = moveToPgn Standard mv pf
            mv = fromJust $ stringToMove moveString
            from = moveFrom mv
            pf = PieceField piece White from

pgnSimpleParse = fmap (\(moveString, piece, pgnString) -> moveTest moveString piece pgnString) fullMoves

-- make sure a game can only be constructed by a function that checks that
-- all moves are legal
-- data Game 


moves = [
      ("e4", "E2E4")
    , ("Nf3", "G1F3")]

testPgnParse :: String -> String -> Test
testPgnParse pgnMove moveString = TestCase $ assertEqual errorString parsedMove pgnMoveParse
    where   parsedMove = stringToMove moveString
            pgnMoveParse = pgnToMove startingGS pgnMove
            errorString = "Pgn game parse"

pgnGameParse = fmap (\(ms, pgnMove) -> testPgnParse ms pgnMove) moves

testCastlingParser = TestCase $ assertEqual "castling rights didn't parse" ((True, True), (True, True)) (castlingRightsParser "KQkq")




-- Can I parse a full game?
-- testResults :: IO [Move]
--     Tu.cd "/home/cg/haskell-chess/test/files"
--     results :: Te.Text <- Tu.strict $ Tu.input "./aronian1.pgn"

tags = [
      ("[Event \"Wch U12\"]", PgnEvent "Wch U12")
    , ("[Site \"SomeSite\"]", PgnSite "SomeSite")
    , ("[Other \"AB\"]", PgnOther "Other" "AB")
    ]

testTag :: String -> PgnTag -> Test
testTag s t = TestCase $ assertEqual "Expected tag" (Right t) parsed
    where parsed = parseOnly fullTagParse (Te.pack s)

testTags = fmap (uncurry testTag) tags

pgnTests = [testPositionParse, testStartingFen, testCastlingParser, testStringTag] ++ pgnSimpleParse ++ pgnGameParse ++ testTags

testStringTag = TestCase $ assertEqual "Expected string tag" (Right expected) parsed
    where parsed = parseOnly (tagParse "Event" $ many' $ letter <|> space <|> digit) $ Te.pack "[Event \"Wch U12\"]"
          expected = "Wch U12"

