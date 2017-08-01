{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
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
import qualified Data.Either.Combinators as EitherC
import Data.List
import Control.Applicative
import qualified Turtle as Tu

-- I can parse a string, from a starting position
moveString = ["e4", "e5", "Ne2", "Nc6", "N2c3"]
expected = ["E2E4", "E7E5", "G1E2", "B8C6", "E2C3"]

gs = parseOnly parseFen (Te.pack startGameFen)
testStartingFen = TestCase $ assertBool ("Starting game state not parsed" ++ show gs) (isRight gs)

parseFirst :: Parser String = do
    positionFen :: String <- many (letter <|> digit <|> (char '/'))
    return positionFen

pos = fenStringToPosition . catMaybes . sequence . maybeResult . (parse parseFirst) . Te.pack $ startGameFen
startingPositionParseTest = length pos == 32 ~? "Starting position parsed with : " ++ (show (length pos)) ++ show pos


fullMoves = [
      ("E2E4", Pawn, "e4")
    , ("G1F3", Knight, "Nf3")]

pgnEqualMoveTest :: String -> Piece -> String -> Test
pgnEqualMoveTest moveString piece pgnString = pgnString ~=? pgnMoveParse
    where   pgnMoveParse = moveToPgn Standard mv pf
            mv = fromJust $ stringToMove moveString
            from = moveFrom mv
            pf = PieceField piece White from

pgnEqualMoveTests = fmap (\(moveString, piece, pgnString) -> pgnEqualMoveTest moveString piece pgnString) fullMoves

moves = [
      ("e4", "E2E4")
    , ("Nf3", "G1F3")]

pgnSameGameAsMove :: String -> String -> Test
pgnSameGameAsMove pgnMove moveString = pgnMoveParse ~=? parsedMove 
    where   parsedMove = stringToMove moveString
            pgnMoveParse = fmap snd $ pgnToMove startingGS pgnMove

pgnSameGameAsMoveTests = fmap (\(ms, pgnMove) -> pgnSameGameAsMove ms pgnMove) moves

testCastlingParser = ((True, True), (True, True)) ~=? castlingRightsParser "KQkq"

tags = [
      ("[Event \"Wch U12\"]\n", PgnEvent "Wch U12")
    , ("[Site \"SomeSite\"]\n", PgnSite "SomeSite")
    , ("[Other \"AB\"]\n", PgnOther "Other" "AB")
    ]

firstTags = Data.List.concat $ fmap fst tags
firstTagsExpected = fmap snd tags

testTag :: String -> PgnTag -> Test
testTag s t = Right t ~=? parseOnly fullTagParse (Te.pack s)

testMultipleTags = Right firstTagsExpected ~=? parseOnly parseAllTags (Te.pack firstTags)

testTags = fmap (uncurry testTag) tags

testStringTag = Right expected ~=? parsed
    where parsed = parser $ Te.pack $ "[Event \"" ++ expected ++ "\"]\n"
          parser = parseOnly (tagParse "Event" $ many' $ letter <|> space <|> digit)
          expected = "Wch U12"

movesNotParse = [
      " e4 e5"
    , "e4 e5"
    , "1. "
    ]

movesNotLegal = [
      "1.e4 e5 2. Ne2 Nf6 3. Nc3"
    , "1.e1 c4"
    , "1.Nf4"
    , "1.Qd3"
    , "1. e4"
    ]

movesGood = [
      "1.e4"
    , "1.e4 e5"
    , "1.e4 e5 2.Nf3"
    , "1.e4 e5 2.Nf3 Nf6 3.Nc3"
    , "1.Nf3"
    , "1.e4 c5 2.Nc3 Nc6 3.f4 d6 4.Nf3 e6 5.Bc4 Nf6 6.d3"
    , "1.e4 d5 2.exd5"
    , "1.e4 d5\n 2.exd5"
    , "1.e4 d5 \n2.exd5"
    , "1.e4 \nd5 \n2.exd5"
    , "1.e4 c5 2.Nc3 Nc6 3.f4 d6 4.Nf3 e6 5.Bc4 Nf6 6.d3 Be7 7.O-O O-O"
    , "1.e4 c5 2.Nc3 Nc6 3.f4 d6 4.Nf3 e6 5.Bc4 Nf6 6.d3 Be7 7.O-O O-O 8.Qe1 a6 9.a4 Qc7 10.Bd2 Rd8 11.e5 dxe5 12.fxe5 Nd5 13.Qg3 Nd4 14.Nxd4 Nxc3 15.bxc3 cxd4 16.cxd4 Rxd4 17.Qf2 Bf8 18.Ba5 Qd7"
    , "1.d4 Nf6 2.Bg5 g6 3.Nf3 Bg7 4.e3 O-O 5.Bc4 d5 6.Bd3 Ne4 7.Bf4 c5 8.c3 Nc6 9.Bxe4 dxe4 10.Ng5 cxd4 11.exd4 e5 12.dxe5 Qxd1+ 13.Kxd1 Bg4+ 14.Kc1 Nxe5 15.Bxe5 Bxe5 16.Nxe4 Bf4+ 17.Kc2 Bf5 18.f3 Rfe8 19.Re1 Bxh2 20.Nd2 Bf4 21.Rad1 Kg7 22.Nb3 Rad8 23.Rxd8 Rxd8 24.Nd4 Re8 25.Kd3 h5 26.b4 Bd7 27.Re2 b6 28.g3 Bc7 29.Rg2 f5 30.Ng5 f4 31.g4 hxg4 32.Rh2 Bf5+ 33.Kd2 g3 34.Nxf5+ gxf5 35.Re2 Be5 36.Kd3 Kg6 37.Nh3 Kf6 38.Rc2 Rh8 39.Ng1 Rh1 40.Rc1 b5 41.c4 bxc4+ 42.Kxc4 Bc7 43.Kd5 Bb6 44.Rc6+ Kg5 45.Ne2 Re1 46.Nc3 g2 47.Rc8 g1=Q 48.Rg8+ Kf6 49.Rxg1 Bxg1 50.a4 Re3 51.Nb5 Rxf3 52.a5 Rb3 53.Kc4 Rb1"
    , "1.d4 d5 2.Bg5 Nf6 3.e3 Bg4 4.Be2 Qd7 5.Nc3 Bxe2 6.Qxe2 Ne4 7.Nxe4 dxe4 8.f3 exf3 9.Nxf3 f6 10.Bf4 e6 11.e4 Be7 12.Qc4 Na6 13.O-O-O O-O-O 14.h4 Rhe8 15.Kb1 Bf8 16.a3 b5 17.Qd3 Qc6 18.c3 Nc5 19.Qc2 Qxe4 20.dxc5 Qxf4 21.Rxd8+ Rxd8 22.Nd4 Qe3 23.b4 a6 24.a4 e5 25.Nc6 Rd3 26.axb5 axb5 27.Na7+ Kb7 28.Nxb5 c6 29.Rd1 e4 30.Na3 Rxc3 31.Nc4 Rxc2 32.Nxe3 Re2 33.Nc4 Kc7 34.Nb6 Bxc5 35.bxc5 Rxg2 36.Rd7+ Kb8 37.Nc4 h5 38.Na5 e3 39.Nxc6+ Kc8 40.Re7 e2 41.Kc1 Rg1+ 42.Kd2 e1=Q+ 43.Rxe1 Rxe1 44.Kxe1 g5 45.hxg5 fxg5 46.Ke2 Kd7 47.Nd4 g4 48.Kd3 h4 49.Nf5 Ke6 50.Ke4 h3 51.Ng3 h2 52.Kd4 Ke7 53.Kd5"
    , "1.d4 d5 2.Bg5 Nf6 3.e3 Bg4 4.Be2 Qd7 5.Nc3 Bxe2 6.Qxe2 Ne4 7.Nxe4 dxe4 8.f3 exf3 9.Nxf3 f6 10.Bf4 e6 11.e4 Be7 12.Qc4 Na6 13.O-O-O"
      ]

testMovesNonParse :: String -> Test
testMovesNonParse s = Nothing ~=? parsed
    where parsedEither = parseOnly parseGameMoves $ Te.pack s
          parsed = EitherC.rightToMaybe parsedEither
          error = "Shouldn't parse single but did: " ++ s ++ " | Parsed" ++ (show parsedEither)

testMovesGood :: String -> Test
testMovesGood s = isJust game ~? error
    where parsedPgnMoves = EitherC.rightToMaybe $ parseOnly parseGameMoves $ Te.pack s -- Maybe [Move]
          game = join $ fmap pgnGame parsedPgnMoves -- Maybe Game
          error = "Read into game failed:" ++ s ++ " | Parsed to: " ++ show parsedPgnMoves


firstMove = fromJust $ stringToMove "E2E4"
secondMove = fromJust $ stringToMove "E7E5"

stateAfterFirst = move startingGS (Pawn, firstMove)
stateAfterSecond = move stateAfterFirst (Pawn, secondMove)

firstToMove = fmap snd $ pgnToMove startingGS "e4"
secondToMove = fmap snd $ pgnToMove stateAfterFirst "e5"


toGameState :: Position -> GameState
toGameState ps = defaultGameState ps White
stringToGs = toGameState . fromJust . stringToPosition
newStateFromMoves gs mvs = fromJust $ tryMoves (Just gs) $ catMaybes $ fmap stringToMove mvs
gsPromote = stringToGs ["WKA1", "BKA8", "WPG7"]

testPromotionParse = TestCase $ assertBool error $ isJust mv
  where mv = pgnToMove gsPromote "g8Q"
        error = "Can parse promotion move"

tagFilter :: Te.Text -> Bool
tagFilter t = not (Te.null t) && (Te.head t == '[')

testExternalPgn = TestCase $ do
    gamePgn :: Te.Text <- Tu.strict $ Tu.input "test/files/manygames.pgn"
    let tagPart = (filter tagFilter $ Te.lines gamePgn) :: [Te.Text]
    let eitherTags = parseOnly parseAllTags $ Te.unlines tagPart
    isRight eitherTags @? "Game tags not read: " ++ show tagPart
    let (_, gamePart) = Te.breakOn "1." gamePgn
    let eitherMoves = parseOnly parseGameMoves gamePart
    isRight eitherMoves @? "Moves are not read: " ++ show gamePart ++ show eitherMoves
    let eitherGameFromMoves = pgnGame $ fromJust $ EitherC.rightToMaybe eitherMoves
    isJust eitherGameFromMoves @? "Moves are not a game: " ++ show eitherGameFromMoves
    let eitherGame = parseOnly parseWholeGame gamePgn
    isRight eitherGame @? "Game is not read: " ++ show eitherGame

testExternalPgns = TestCase $ do
  gamePgnRaw :: Te.Text <- Tu.strict $ Tu.input "test/files/many.pgn"
  let number = 1
  let needle = "[Event"
  let gamePgn = Te.intercalate needle $ take (number + 1) $ Te.splitOn needle gamePgnRaw
  let eitherGame = parseOnly parseWholeGames gamePgn
  isRight eitherGame @? "Game is not read: " ++ show eitherGame
  let games = catMaybes $ fromJust $ EitherC.rightToMaybe eitherGame
  assertEqual ("Not all games parsed" ++ show games) number (length games)


singleTests = [
    startingPositionParseTest
  , testStartingFen
  , testCastlingParser
  , testStringTag
  , testMultipleTags
  , "First move not parsed" ~: Just firstMove ~=? firstToMove
  , "Second move not parsed" ~: Just secondMove ~=? secondToMove
  ]

promotionTests = [
    "PGN can read promotion move" ~: "e8N" ~: ("e8", Just Knight) ~=? (pgnToPromotion "e8N")
  , "PGN can read promotion move" ~: "e8Q" ~: ("e8", Just Queen) ~=? (pgnToPromotion "e8Q")
  , "Can parse promotion move" ~: testPromotionParse]

externalFileTests = [
    "Cannot read file with one PGN: " ~: testExternalPgn
  , "Cannot read file with many PGNs: " ~: testExternalPgns
  ]

moveListTests = [
    "Moves that should parse: " ~: fmap testMovesGood movesGood
  , "Moves that should not parse: " ~: fmap testMovesNonParse movesNotParse
  ]

pgnParseTests = pgnEqualMoveTests ++ pgnSameGameAsMoveTests

pgnTests = [
    "Promotion tests: " ~: promotionTests
  , "External file tests: " ~: externalFileTests
  , "Assorted tests: " ~: singleTests
  -- , "Pgn game parsing tests:" ~: pgnParseTests
  , "Tag parsing" ~: testTags
  -- , "Parsing move lists" ~: moveListTests
  ]

-- 86 tests total

