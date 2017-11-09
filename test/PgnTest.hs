{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
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
import Chess.Pgn

-- I can parse a string, from a starting position

moveString = ["e4", "e5", "Ne2", "Nc6", "N2c3"]
expected = ["E2E4", "E7E5", "G1E2", "B8C6", "E2C3"]

pos = (view gsPosition) . fromJust . fenToGameState $ startGameFen
startingPositionParseTest = 32 ~=? length pos

fullMoves = [
      ("E2E4", Pawn, "e4")
    , ("G1F3", Knight, "Nf3")]

pgnEqualMoveTest :: String -> Piece -> String -> Test
pgnEqualMoveTest moveString piece pgnString = pgnString ~=? pgnMoveParse
    where   pgnMoveParse = moveToPgn Standard mv pf
            mv = fromJust $ stringToMove moveString
            from = mv ^. moveFrom
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
    , ("[Event \"?\"]\n", PgnEvent "?")
    , ("[Site \"SomeSite\"]\n", PgnSite "SomeSite")
    , ("[Other \"AB\"]\n", PgnOther "Other" "AB")
    , ("[White \"Bo, Anna\"]\n", PgnWhite (Player "Anna" "Bo"))
    , ("[White \"Bo\"]\n", PgnWhite (Player "" "Bo"))
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
    , "1.d4 d5 2.Bg5 Nf6 3.e3 Bg4 4.Be2 Qd7 5.Nc3 Bxe2 6.Qxe2 Ne4 7.Nxe4 dxe4 8.f3 exf3 9.Nxf3 f6 10.Bf4 e6 11.e4 Be7 12.Qc4 Na6 13.O-O-O O-O-O 14.h4 Rhe8 15.Kb1 Bf8 16.a3 b5 17.Qd3 Qc6 18.c3 Nc5 19.Qc2 Qxe4 20.dxc5 Qxf4 21.Rxd8+ Rxd8"
    , "1. e4 c5 2. Nf3 d6 3. d4 cxd4 4. Nxd4 Nf6 5. Nc3 g6 6. g3 Bg7 7. Bg2 O-O 8. O-O a6 9. a4 Bg4 10. f3 Bd7 11. Be3 Nc6 12. Qd2 Ne5 13. b3 Rc8 14. a5 Qc7 15. Nde2 Bc6 16. Bb6 Qb8 17. Nd5 Rfe8 18. c4 Ned7 19. Bd4 Bxd5 20. exd5 Nc5 21. Ra3 e5 22. dxe6 fxe6 23. Bh3 Rcd8 24. b4 Ncd7 25. Rd1 Kf7 26. Rd3 d5 27. cxd5 exd5 28.  Bxd7 Rxd7 29. Bxf6 Bxf6 30. Rxd5 Rde7 31. Rd7 Qc8 32. Qd5+ Kg7 33. Nf4 Kh6 34.  Rxe7 Rxe7 35. Nh3 Qxh3 0-1"
    , "1.d4 Nf6 2.Bg5 d5 3.Bxf6 exf6 4.e3 g6 5.c4 Bg7 6.Nc3 dxc4 7.Bxc4 O-O 8.h4 c5 9.h5 cxd4 10.hxg6 hxg6 11.exd4 Nc6 12.Nge2 f5 13.Qd3 Re8 14.O-O-O Na5 15.Bd5 Bd7 16.g4 Rc8 17.Kb1 Nc6 18.Qf3 Qf6 19.g5 Qxg5 20.Rhg1 Qf6 21.Nf4 Kf8 22.Nce2 Qd6 23.Nxg6+ fxg6 24.Nf4 Ne7 25.Bb3 Bh6 26.Nd3 Bc6 27.Qh3 Bg7 28.Nc1 Bd5 29.Ba4 Red8 30.f3 Bf7"
    , "1.d4 Nf6 2.Bg5 d5 3.Bxf6 exf6 4.e3 g6 5.c4 Bg7 6.Nc3 dxc4 7.Bxc4 O-O 8.h4 c5 9.h5 cxd4 10.hxg6 hxg6 11.exd4 Nc6 12.Nge2 f5 13.Qd3 Re8 14.O-O-O Na5 15.Bd5 Bd7 16.g4 Rc8 17.Kb1 Nc6 18.Qf3 Qf6 19.g5 Qxg5 20.Rhg1 Qf6 21.Nf4 Kf8 22.Nce2 Qd6 23.Nxg6+ fxg6 24.Nf4 Ne7 25.Bb3 Bh6 26.Nd3 Bc6 27.Qh3 Bg7"
    , "1.d4 Nf6 2.Bg5 d5 3.Bxf6 exf6 4.e3 g6 5.c4 Bg7 6.Nc3 dxc4 7.Bxc4 O-O 8.h4 c5 9.h5 cxd4 10.hxg6 hxg6 11.exd4 Nc6 12.Nge2 f5 13.Qd3 Re8 14.O-O-O Na5 15.Bd5 Bd7 16.g4 Rc8 17.Kb1 Nc6 18.Qf3 Qf6 19.g5 Qxg5 20.Rhg1 Qf6 21.Nf4 Kf8 22.Nce2 Qd6 23.Nxg6+ fxg6"
    , "1.d4 Nf6 2.Bg5 d5 3.Bxf6 exf6 4.e3 g6 5.c4 Bg7 6.Nc3 dxc4 7.Bxc4 O-O 8.h4 c5 9.h5 cxd4 10.hxg6 hxg6 11.exd4 Nc6 12.Nge2 f5 13.Qd3 Re8 14.O-O-O Na5 15.Bd5 Bd7 16.g4 Rc8 17.Kb1 Nc6 18.Qf3 Qf6 19.g5 Qxg5 20.Rhg1 Qf6 21.Nf4 Kf8"
    , "1.d4 Nf6 2.Bg5 d5 3.Bxf6 exf6 4.e3 g6 5.c4 Bg7 6.Nc3 dxc4 7.Bxc4 O-O 8.h4 c5 9.h5 cxd4 10.hxg6 hxg6 11.exd4 Nc6 12.Nge2 f5"
    , "1.d4 Nf6 2.Bg5 d5 3.Nd2 c6 4.e3 Bf5 5.Bd3 Bg6 6.Ngf3 Nbd7 7.Qe2 Ne4 8.Bxe4 dxe4 9.Nh4 Qa5 10.Nxg6 hxg6 11.Bf4 O-O-O 12.c3 g5 13.Bg3 Qf5 14.a4 e5 15.a5 a6 16.Qc4 Kb8 17.dxe5 Nc5 18.O-O-O Kc8 19.b4 Nd3+ 20.Kb1 Nxe5 21.Qxe4 Qg6 22.Bxe5 Rxd2 23.Qxg6 Rxd1+ 24.Rxd1 fxg6 25.h3 Rh5 26.Bd6 Bxd6 27.Rxd6 Rh6 28.f3 Kc7 29.Rd4 Rh8 30.Kc2 Re8 31.Kd3 Re5 32.Re4 Rd5+ 33.Kc4 Rd2 34.Rg4 Rd5 35.e4 Re5 36.h4  1-0"
    , "1.d4 Nf6 2.Bg5 d5 3.Nd2 c6 4.e3 Bf5 5.Bd3 Bg6 6.Ngf3 Nbd7 7.Qe2 Ne4 8.Bxe4 dxe4 9.Nh4 Qa5 10.Nxg6 hxg6 11.Bf4 O-O-O 12.c3 g5 13.Bg3 Qf5 14.a4 e5 15.a5 a6 16.Qc4 Kb8 17.dxe5 Nc5 18.O-O-O Kc8 19.b4 Nd3+ 20.Kb1 Nxe5 21.Qxe4 Qg6 22.Bxe5 Rxd2 23.Qxg6 Rxd1+ 24.Rxd1 fxg6 25.h3 Rh5 26.Bd6 Bxd6 27.Rxd6 Rh6 28.f3 Kc7 29.Rd4 Rh8"
    , "1.d4 Nf6 2.Bg5 d5 3.Nd2 c6 4.e3 Bf5 5.Bd3 Bg6 6.Ngf3 Nbd7 7.Qe2 Ne4 8.Bxe4 dxe4 9.Nh4 Qa5 10.Nxg6 hxg6 11.Bf4 O-O-O 12.c3 g5"
    , "1.d4 Nf6 2.Bg5 d5 3.Nd2 c6 4.e3 Bf5 5.Bd3 Bg6 6.Ngf3 Nbd7 7.Qe2 Ne4 8.Bxe4 dxe4 9.Nh4 Qa5 10.Nxg6 hxg6 11.Bf4"
    , "1.d4 Nf6 2.Bg5 d5 3.Nd2 c6 4.e3 Bf5 5.Bd3 Bg6"
    , "1.d4 Nf6 2.Bg5 g6 3.Bxf6 exf6 4.e3 Bg7 5.g3 d5 6.Bg2 c6 7.Ne2 O-O 8.Nd2 Nd7 9.c4 dxc4 10.Nxc4 Nb6 11.Na5 Nd5 12.Qd2 f5 13.b4 Re8 14.Rc1 a6 15.a3 Re7 16.Rxc6 Bh6 17.Rc5 Nxe3 18.fxe3 Bxe3 19.Qd3 f4 20.gxf4 Bg4 21.Re5 Rxe5 22.fxe5 Bh6 23.Qe4 Bf5 24.Qxb7 Qh4+ 25.Ng3 Rc8 26.Nc6 Rxc6 27.Qxc6 Qxd4 28.Qa8+ Kg7 29.Nxf5+ gxf5 30.Qf3 Qd2+ 31.Kf1 Be3 32.Qg3+ Kf8 33.Bf3 f4 34.Qe1 Qd3+ 35.Qe2 Qb1+ 36.Kg2 Qg6+ 37.Kh3  1-0"
      ]

testMovesNonParse :: String -> Test
testMovesNonParse s = Nothing ~=? parsed
    where parsedEither = parseOnly parseGameMoves $ Te.pack s
          parsed = EitherC.rightToMaybe parsedEither
          error = "Shouldn't parse single but did: " ++ s ++ " | Parsed" ++ (show parsedEither)

testMovesGood :: String -> Test
testMovesGood s = isRight game ~? error
    where parsedPgnMoves = parseOnly parseGameMoves $ Te.pack s -- Either String [Move]
          toEitherGame (Left s) = Left s
          toEitherGame (Right m) = Right $ pgnGame m
          game = toEitherGame parsedPgnMoves
          error = "Read into game failed:" ++ s ++ " | Parsed to: " ++ show parsedPgnMoves


fullParse = [
    ("1. e4 e5 (1... e6 $22 $18 {[%emt 1:40:00] comment} 2. f4 (2. c4 $13)) 2. d4 {comment with , and # and $ and ()} c5 *", 4)
  , ("1.e4 (1.Nf4 (..Nf6) ((1.d4) (1.Nf4))) 1... e5 2.Nf3 Nf6", 4)
  , ("1. Ng6 (1. Bg3 Qb6 () ) 1... Qb6 2. Bxc6+", 3)]

testFullParse :: (String, Int) -> Test
testFullParse (s, num) = correct ~? error
    where parsedPgnMoves = EitherC.rightToMaybe $ parseOnly parseGameMoves $ Te.pack s -- Maybe [Move]
          error = "Read into moves failed:" ++ s ++ " | Parsed to: " ++ show parsedPgnMoves
          correct = length (fromJust parsedPgnMoves) == num

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
    isRight eitherGameFromMoves @? "Moves are not a game: " ++ show eitherGameFromMoves
    let eitherGame = readSingleGame gamePgn
    isRight eitherGame @? "Game is not read: " ++ show eitherGame

testExternalPgns = TestCase $ do
  let file = "test/files/many.pgn"
  let number = 1
  games <- getGames file number
  let (lefts, rights) = Data.List.span Data.Either.isLeft games
  let total = length games
  assertEqual ((show total) ++  "games. Not all games parsed" ++ show lefts) number (length rights)


singleTests = [
    startingPositionParseTest
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
  ]

-- 86 tests total

