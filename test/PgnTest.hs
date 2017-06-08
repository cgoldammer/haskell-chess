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
import qualified Data.Text as T
import Control.Monad
import Data.Maybe
import Data.Either
import Control.Applicative

-- I can parse a string, from a starting position
moveString = ["e4", "e5", "Ne2", "Nc6", "N2c3"]
expected = ["E2E4", "E7E5", "G1E2", "B8C6", "E2C3"]

gs :: Either String GameState
gs = parseOnly parseFen (T.pack startGameFen)
testStartingFen = TestCase $ assertBool ("Starting game state not parsed" ++ show gs) (isRight gs)

parseFirst :: Parser String = do
    positionFen :: String <- many (letter <|> digit <|> (char '/'))
    return positionFen

pos = fenStringToPosition . catMaybes . sequence . maybeResult . (parse parseFirst) . T.pack $ startGameFen
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

pgnTests = [testPositionParse, testStartingFen, testCastlingParser] ++ pgnGameParse ++ pgnSimpleParse
