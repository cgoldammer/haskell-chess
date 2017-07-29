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
testPositionParse = TestCase $ assertBool ("Starting position parsed with : " ++ (show (length pos)) ++ show pos) (length pos == 32)


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
            pgnMoveParse = fmap snd $ pgnToMove startingGS pgnMove
            errorString = "Pgn game parse"

pgnGameParse = fmap (\(ms, pgnMove) -> testPgnParse ms pgnMove) moves

testCastlingParser = TestCase $ assertEqual "castling rights didn't parse" ((True, True), (True, True)) (castlingRightsParser "KQkq")




-- Can I parse a full game?
-- testResults :: IO [Move]
--     Tu.cd "/home/cg/haskell-chess/test/files"
--     results :: Te.Text <- Tu.strict $ Tu.input "./aronian1.pgn"

tags = [
      ("[Event \"Wch U12\"]\n", PgnEvent "Wch U12")
    , ("[Site \"SomeSite\"]\n", PgnSite "SomeSite")
    , ("[Other \"AB\"]\n", PgnOther "Other" "AB")
    ]

firstTags = Data.List.concat $ fmap fst tags
firstTagsExpected = fmap snd tags

testTag :: String -> PgnTag -> Test
testTag s t = TestCase $ assertEqual "Expected tag" (Right t) parsed
    where parsed = parseOnly fullTagParse (Te.pack s)

testMultipleTags = TestCase $ assertEqual "Multiple tags" (Right firstTagsExpected) parsed
    where parsed = parseOnly parseAllTags $ Te.pack firstTags

testTags = fmap (uncurry testTag) tags


testStringTag = TestCase $ assertEqual "Expected string tag" (Right expected) parsed
    where parsed = parseOnly (tagParse "Event" $ many' $ letter <|> space <|> digit) $ Te.pack "[Event \"Wch U12\"]\n"
          expected = "Wch U12"

movesNotParse = [
      " e4 e5"
    , "e4 e5"
    , "1. "
    ]

movesBad = [
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
    -- , "1. e4 Nf6 2. e5 Ne4 3. d3 Nc5 4. d4 Ne4 5. Qd3 d5 6. exd6 Nxd6 7. Nf3 b5 8. Bf4 e5 9. Bxe5 Bf5 10. Qb3 Nc6 11. Bxb5 Qd7 12. O-O Ne4 13. Nc3 a6 14. Ba4 Be6 15. d5 Bf5 16. Bxc6 Qxc6 17. dxc6 Bc5 18. Bxg7 Rg8 19. Ne5 Rxg7 20. Nxe4 Bxe4 21. g3 f5 22. Rad1 Bf3 23. Rd7 Rd8 24. Rxg7 Rd4 25. Qf7+ Kd8 26. Qg8+ Bf8 27. Qxf8#"  
    , "1.Nf3"
    , "1.e4 c5 2.Nc3 Nc6 3.f4 d6 4.Nf3 e6 5.Bc4 Nf6 6.d3"
    , "1.e4 d5 2.exd5"
    -- , "1.e4 c5 2.Nc3 Nc6 3.f4 d6 4.Nf3 e6 5.Bc4 Nf6 6.d3 Be7 7.O-O O-O"
      ]

testMovesBad :: String -> Test
testMovesBad s = TestCase $ assertEqual error Nothing parsed
    where parsedEither = parseOnly parseGameMoves $ Te.pack s
          parsed = EitherC.rightToMaybe parsedEither
          error = "Shouldn't parse single but did: " ++ s ++ " | Parsed" ++ (show parsedEither)

testSingleMove = TestCase $ assertEqual error Nothing parsed
    where parsedEither = parseOnly bothMoveParser $ Te.pack mv
          parsed = EitherC.rightToMaybe parsedEither
          mv = "e4 e5"
          error = "Shouldn't parse single but did: " ++ mv ++ " | Parsed" ++ (show parsedEither)
          

testMovesGood :: String -> Test
testMovesGood s = TestCase $ assertBool error $ isJust game
    where parsedPgnMoves = EitherC.rightToMaybe $ parseOnly parseGameMoves $ Te.pack s -- Maybe [Move]
          game = join $ fmap pgnGame parsedPgnMoves -- Maybe Game
          error = "Read into game failed:" ++ s ++ " | Parsed to: " ++ show parsedPgnMoves


firstMove = fromJust $ stringToMove "E2E4"
secondMove = fromJust $ stringToMove "E7E5"

stateAfterFirst = move startingGS (Pawn, firstMove)
stateAfterSecond = move stateAfterFirst (Pawn, secondMove)

firstToMove = fmap snd $ pgnToMove startingGS "e4"
secondToMove = fmap snd $ pgnToMove stateAfterFirst "e5"

testFirst = TestCase $ assertEqual "Expected first move" (Just firstMove) firstToMove
testSecond = TestCase $ assertEqual "Expected second move" (Just secondMove) secondToMove

tagFilter :: Te.Text -> Bool
tagFilter t = not (Te.null t) && (Te.head t == '[')

testExternalPgn = TestCase $ do
    gamePgn :: Te.Text <- Tu.strict $ Tu.input "test/files/aronian1.pgn"
    let tagPart = (filter tagFilter $ Te.lines gamePgn) :: [Te.Text]
    let eitherTags = parseOnly parseAllTags $ Te.unlines tagPart
    assertBool ("Game tags not read: " ++ show tagPart) (isRight eitherTags)
    let (_, gamePart) = Te.breakOn "1." gamePgn
    let eitherMoves = parseOnly parseGameMoves gamePart
    assertBool ("Moves are not read: " ++ show gamePart ++ show eitherMoves) (isRight eitherMoves)
    let eitherGame = parseOnly parseWholeGame gamePgn
    assertBool ("Game is not read: " ++ show eitherGame) (isRight eitherGame)
    let maybeGame = either (const Nothing) id eitherGame
    assertBool ("Game is not parsed: " ++ (show maybeGame)) (1 == 2)

singleTests = [
      testPositionParse
    , testStartingFen
    , testCastlingParser
    , testStringTag
    , testMultipleTags
    , testSingleMove
    , testFirst
    -- , testExternalPgn
    , testSecond]

testsMoveGood = fmap testMovesGood movesGood
testsMoveNonParse = fmap testMovesBad movesNotParse
testMoves = testsMoveGood ++ testsMoveNonParse

pgnParseTests = pgnSimpleParse ++ pgnGameParse

pgnTests = [] -- singleTests -- ++ pgnParseTests ++ testTags ++ testMoves



