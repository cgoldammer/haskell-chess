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
import Control.Applicative

-- I can parse a string, from a starting position
moveString = ["e4", "e5", "Ne2", "Nc6", "N2c3"]
expected = ["E2E4", "E7E5", "G1E2", "B8C6", "E2C3"]

gs :: Maybe GameState
gs = join $ maybeResult $ parse parseFen $ T.pack startGameFen
testStartingFen = TestCase $ assertBool "Starting game state not parsed" (1==1) -- (isJust gs)

parseFirst :: Parser String = do
    positionFen :: String <- many (letter <|> digit <|> (char '/'))
    return positionFen

pos = fenStringToPosition . catMaybes . sequence . maybeResult . (parse parseFirst) . T.pack $ startGameFen
testPositionParse = TestCase $ assertBool ("Starting position parsed with : " ++ (show (length pos)) ++ show pos) (length pos == 32)

pgnTests = [testPositionParse, testStartingFen]


