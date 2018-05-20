module PgnTest (pgnTests) where

import Control.Lens
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
import Data.List
import Control.Applicative
import qualified Turtle as Tu

import Chess.Algorithms
import Chess.Board
import Chess.Logic
import Chess.Pgn.Logic
import Chess.Pgn.External
import qualified Data.Traversable as Tr
import qualified System.IO.Unsafe as Unsafe
-- I can parse a string, from a starting position

moveString = ["e4", "e5", "Ne2", "Nc6", "N2c3"]
expected = ["E2E4", "E7E5", "G1E2", "B8C6", "E2C3"]

startGameFen = "fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
pos = (view gsPosition) . fromJust . fenToGameState $ startGameFen
startingPositionParseTest = sort (startingGS ^. gsPosition) ~=? sort pos

fullMoves = [
      ("E2E4", Pawn, "e4")
    , ("G1F3", Knight, "Nf3")]

pgnEqualMoveTest :: String -> Piece -> String -> Test
pgnEqualMoveTest moveString piece pgnString = pgnString ~=? pgnMoveParse
    where   pgnMoveParse = moveToPgn False False False Standard mv pf
            mv = snd $ fromJust $ stringToMove startingGS moveString -- Move
            from = mv ^. moveFrom
            pf = PieceField piece White from

pgnEqualMoveTests = fmap (\(moveString, piece, pgnString) -> pgnEqualMoveTest moveString piece pgnString) fullMoves

moves = [
      ("e4", "E2E4")
    , ("Nf3", "G1F3")]


pgnSameGameAsMove :: String -> String -> Test
pgnSameGameAsMove pgnMove moveString = pgnMoveParse ~=? parsedMove 
    where   parsedMove = stringToMove startingGS moveString
            pgnMoveParse = pgnToMove startingGS pgnMove

pgnSameGameAsMoveTests = fmap (\(ms, pgnMove) -> pgnSameGameAsMove ms pgnMove) moves

testCastlingParser = castleAll ~=? castlingRightsParser "KQkq"

tags = [
      ("[Event \"Wch U12\"]\n", PgnEvent "Wch U12")
    , ("[Event \"Some - ? Other\"]\n", PgnEvent "Some - ? Other")
    , ("[Event \"Tata Steel-A 78th\"]\n", PgnEvent "Tata Steel-A 78th")
    , ("[Date \"2016.01.16\"]\n", PgnDate "2016.01.16")
    , ("[WhiteElo \"1001\"]\n", PgnWhiteElo 1001)
    , ("[Result \"1-0\"]\n", PgnResult WhiteWin)
    , ("[Result \"0-1\"]\n", PgnResult BlackWin)
    , ("[Result \"1/2-1/2\"]\n", PgnResult Draw)
    , ("[Event \"?\"]\n", PgnEvent "?")
    , ("[Site \"SomeSite\"]\n", PgnSite "SomeSite")
    , ("[Other \"AB\"]\n", PgnOther "Other" "AB")
    , ("[White \"Bo, Anna\"]\n", PgnWhite (Player "Anna" "Bo"))
    , ("[White \"Van Wely, Loek\"]\n", PgnWhite (Player "Loek" "Van Wely"))
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


testMovesNonParse :: String -> Test
testMovesNonParse s = Nothing ~=? parsed
    where parsedEither = parseOnly parseGameMoves $ Te.pack s
          parsed = EitherC.rightToMaybe parsedEither
          error = "Shouldn't parse single but did: " ++ s ++ " | Parsed" ++ (show parsedEither)


testMovesGood :: String -> Test
testMovesGood s = isRight game ~? error
    where parsedPgnMoves = parseOnly parseGameMoves $ Te.pack s -- Either String [Move]
          game = fmap (gameFromStart pgnToMove) parsedPgnMoves
          error = "Read into game failed:" ++ show s ++ " Game: " ++ show game ++ " | Parsed to: " ++ show parsedPgnMoves ++ show game


fullParse = [
    ("1. e4 e5 (1... e6 $22 $18 {[%emt 1:40:00] comment} 2. f4 (2. c4 $13)) 2. d4 {comment with , and # and $ and ()} c5 *", 4)
  , ("1.e4 (1.Nf4 (..Nf6) ((1.d4) (1.Nf4))) 1... e5 2.Nf3 Nf6", 4)
  , ("1. Ng6 (1. Bg3 Qb6 () ) 1... Qb6 2. Bxc6+", 3)]

testFullParse :: (String, Int) -> Test
testFullParse (s, num) = correct ~? error
    where parsedPgnMoves = EitherC.rightToMaybe $ parseOnly parseGameMoves $ Te.pack s -- Maybe [Move]
          error = "Read into moves failed:" ++ s ++ " | Parsed to: " ++ show parsedPgnMoves
          correct = length (fromJust parsedPgnMoves) == num

toGameState :: Position -> GameState
toGameState ps = defaultGameStateNoCastle ps White

stringToGs :: [String] -> GameState
stringToGs = toGameState . fromJust . stringToPosition

newStateFromMoves :: GameState -> [String] -> GameState
newStateFromMoves gs mvs = last $ gameStates $ fromJust $ EitherC.rightToMaybe game
  where game = gameFromString pgnToMove gs mvs

gsPromote = stringToGs ["WKA1", "BKA8", "WPG7"]

testPromotionParse = TestCase $ assertBool error $ isJust mv
  where mv = pgnToMove gsPromote "g8=Q+"
        error = "Can parse promotion move"

tagFilter :: Te.Text -> Bool
tagFilter t = not (Te.null t) && (Te.head t == '[')

testExternalPgnsFile fileName = TestCase $ do
  let file = "test/files/" ++ fileName
  text <- readGameText file 
  let error = "Text length: " ++ show (Te.length text)
  let games = getGamesFromText text
  let (lefts, rights) = Data.List.span Data.Either.isLeft games
  let total = length games
  let error = ((show total) ++ " games. Not all games parsed" ++ show lefts)
  assertEqual error total (length rights)


testToPgn :: GameState -> [String] -> Test
testToPgn gs pgnMoves = (show gs) ~: pgnMoves ~=? actual
  where game = fromJust $ EitherC.rightToMaybe $ gameFromString pgnToMove gs pgnMoves
        actual = gamePgnMoves game

toPgnData :: [([String], [String])]
toPgnData = [
    (["WKA1", "WNB1", "BKA8"], ["Nc3"])
  , (["WKA1", "WNB1", "BKA8", "WND1"], ["Ndc3"])
  , (["WKA1", "WNB1", "BKA8", "WND5"], ["Nbc3"])
  , (["WKA1", "WNB1", "BKA8", "WNB5"], ["N1c3"])
  , (["WKA1", "WNB1", "BKA8", "WNB5", "WND1", "WND5"], ["Nb1c3"])
  , (["WKA1", "BKA8", "WRB2"], ["Ra2+"])
  , (["WKA1", "BKA8", "WRB2", "WRC3"], ["Ra3#"])
  ]

toPgnDataCastles = [
    (["WKE1", "WRH1", "BKA8"], ["O-O"], \s -> defaultGameState (stringPos s) White)
  , (["WKE1", "WRH1", "BKF8"], ["O-O+"], \s -> defaultGameState (stringPos s) White)
  , (["WKE1", "BKE8", "BRA8"], ["O-O-O"], \s -> defaultGameState (stringPos s) Black)]

toPgnDataPromotes = [
    (["WKA1", "BKA8", "WPC7"], ["c8=Q+"], \s -> defaultGameStateNoCastle (stringPos s) White)
  , (["WKE1", "BKA8", "WPC7", "BND8"], ["cxd8=B"], \s -> defaultGameStateNoCastle (stringPos s) White)
  , (["WKE1", "BKE8", "BPC2"], ["c1=Q+"], \s -> defaultGameStateNoCastle (stringPos s) Black)]

stringPos = fromJust . stringToPosition

toPgnTests = fmap (\(posString, mv) -> testToPgn (stringToGs posString) mv) toPgnData

createExpandedPgnTest = fmap (\(posString, mv, gsFun) -> testToPgn (gsFun posString) mv)

toPgnTestsCastles = createExpandedPgnTest toPgnDataCastles
toPgnTestsPromotes = createExpandedPgnTest toPgnDataPromotes

singleTests = [
    "Starting position parses correctly" ~: startingPositionParseTest
  , "Castling parses correctly" ~: testCastlingParser
  , "String tags are read correctly" ~: testStringTag
  , "Multiple tags are read correctly" ~: testMultipleTags
  ]

promotionTests = [
    "PGN can read promotion move" ~: "e8N" ~: ("e8", Just Knight) ~=? (pgnToPromotion "e8N")
  , "PGN can read promotion move" ~: "e8Q" ~: ("e8", Just Queen) ~=? (pgnToPromotion "e8Q")
  , "Can parse promotion move" ~: testPromotionParse]

externalFileTests = [
  "Cannot read file with many PGNs: " ~: testExternalPgnsFile "many.pgn"
  ]


validStrings :: [String]
validStrings = init $ (fmap Te.unpack) . (Te.splitOn (Te.pack "\n")) $ games
  where games = Unsafe.unsafePerformIO $ readGameText "test/files/validGames.txt"


moveListTests = [
    "Moves that should parse: " ~: fmap testMovesGood validStrings
  , "Moves that should not parse: " ~: fmap testMovesNonParse movesNotParse
  , "Moves that should fully parse: " ~: fmap testFullParse fullParse
  ]

pgnParseTests = pgnEqualMoveTests ++ pgnSameGameAsMoveTests

pgnTests = [
    "Promotion tests: " ~: promotionTests
  , "External file tests: " ~: externalFileTests
  , "Assorted tests: " ~: singleTests
  , "Pgn game parsing tests:" ~: pgnParseTests
  , "Tag parsing" ~: testTags
  , "Parsing move lists" ~: moveListTests
  , "Exporting PGN works correctly" ~: toPgnTests
  , "Exporting PGN Castling works correctly" ~: toPgnTestsCastles
  , "Exporting PGN Promotion works correctly" ~: toPgnTestsPromotes
  ]

gm = "1.d4 Nf6 2.Bg5 c5 3.e3 Qb6 4.Nc3 e6 5.dxc5 Bxc5 6.Rb1 d5 7.Qf3 Nbd7 8.Bb5 Ne4 9.Nxe4 dxe4 10.Qe2 a6 11.Bxd7+ Bxd7 12.Qd2 f6 13.Bh4 Rd8 14.Ne2 Bb5 15.Qc1 Bb4+ 16.c3 Bxe2 17.Kxe2 Qb5+ 18.Ke1 Be7 19.Qc2 Rd3 20.Qb3 Qd7 21.Rd1 Kf7 22.Rxd3 Qxd3 23.Qd1 Qb5 24.Qe2 Qd5 25.b3 Rd8 26.f4 exf3 27.gxf3 Qc5 28.Kf2 Qxc3 29.Rd1 Rxd1 30.Qxd1 Qb2+ 31.Qe2 Qxe2+ 32.Kxe2 Bd6 33.h3 Be5 34.Be1 Ke7 35.Kd3 Kd7 36.a4 Kc6 37.Kc4 Bd6 38.Bh4 Bc5 39.e4 Be3 40.Be1 Bf4 41.Bf2 Bd6 42.Kd3 Be5 43.Kc4 g5 44.Kd3 Bf4 45.Bd4 f5 46.Ke2 h5 47.Kf1 Kd6 48.Ke2 Kc6 49.Ke1 Bd6 50.Ke2 Bc7 51.Be3 g4 52.fxg4 hxg4 53.hxg4 fxg4 54.Kf1 Bb6 55.Bh6 Bd4 56.Bf8 Be5 57.Kg2 Bd6 58.Bg7 Kc5 59.Bc3 b6 60.Kf2 Bf4 61.Kg2 b5 62.axb5 Kxb5 63.Kf2 Bc7 64.Bd2 Ba5 65.Bf4 Bb6+ 66.Kg3 Kb4 67.Be5 Kxb3 68.Kxg4 Kc4 69.Kg5 a5 70.Kg6 Bd4 71.Bc7 a4 72.Bd6 e5 73.Kf5 Kb3  0-1"
