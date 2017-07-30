{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}

module Pgn (
    startGameFen
  , pgnToMove, cleanPgn, pgnAsMoves
  , PgnType (Standard, WithColumn, WithRow, WithBoth)
  , possibleMoveFields
  , pgnPiece
  , pgnToTargetField, pgnToPromotion
  , PgnTag (..)
  , tagParse
  , eventParse, siteParse, fullTagParse, parseAllTags
  , parseGameMoves, moveParser, bothMoveParser, parseWholeGame, parseWholeGames
  , pgnGame
  , startingGS
  , moveToPgn) where

import Board
import Logic
import Helpers

import Control.Applicative
import Data.Aeson
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.Char as Ch
import Data.List
import Data.Maybe
import qualified Data.Text as Te
import Data.Either
import Data.Either.Combinators as EitherC

import Board
import Logic
import Various
import Debug.Trace

type PgnMove = String

pgnParse :: PgnMove -> Color -> PgnMove
pgnParse "O-O" White = "Kg1"
pgnParse "O-O-O" White = "Kc1"
pgnParse "O-O" Black = "Kg8"
pgnParse "O-O-O" Black = "Kc8"
pgnParse p _ = p

pgnToPromotion :: PgnMove -> (PgnMove, Maybe Piece)
pgnToPromotion pgn
  | end `elem` ("QNBR" :: String) = (init pgn, stringToPiece [end])
  | otherwise = (pgn, Nothing)
  where end = last pgn

cleanPgn :: PgnMove -> Color -> PgnMove
cleanPgn pgn color = withoutPromotion
  where (withoutPromotion, _) = pgnToPromotion $ pgnParse pgn color

possibleMoveFields :: GameState -> PgnMove -> [((Piece, Move), [String])]
possibleMoveFields gs pgn = zip movesWithPiece pgnMoves
    where   
            allMoves = filter (\(piece, _) -> piece == movePiece) $ allNextLegalMoves gs
            (pgnWithoutPromotion, promotionPiece) = pgnToPromotion pgn
            targetField = pgnToTargetField pgnWithoutPromotion
            movePiece = pgnPiece pgnWithoutPromotion
            color = gsColor gs
            movesWithPieceFields = [(mv, PieceField piece color (moveFrom mv)) | (piece, mv) <- allMoves, targetField == Just (moveTo mv) && promotionPiece == movePromotionPiece mv]
            movesWithPiece = [(pfPiece pf, m) | (m, pf) <- movesWithPieceFields]
            pgnMoves = createPgnMoves gs movesWithPieceFields

pgnToTargetField pgn = stringToField $ fmap Ch.toUpper $ reverse $ Data.List.take 2 $ reverse pgn

pgnToMove :: GameState -> PgnMove -> Maybe (Piece, Move)
pgnToMove gs pgn = listToMaybe [mv | (mv, pgnList) <- possibleMoveFields gs (pgnParse pgn color), pgnCleaned `elem` pgnList]
    where color = gsColor gs
          pgnCleaned = cleanPgn pgn color

pgnPiece :: PgnMove -> Piece
pgnPiece pgnMove
  | head pgnMove `elem` ("KQRNB" :: String) = fromJust $ stringToPiece $ [head pgnMove]
  | otherwise = Pawn

createPgnMoves :: GameState -> [(Move, PieceField)] -> [[PgnMove]]
createPgnMoves gs ls = fmap (\el -> expandMove gs (fst el) (snd el)) ls

expandMove :: GameState -> Move -> PieceField -> [PgnMove]
expandMove gs mv pf = [withNeither, withColumn, withRow, withBoth]
    where   withColumn = moveToPgn WithColumn mv pf
            withRow = moveToPgn WithRow mv pf
            withBoth = moveToPgn WithBoth mv pf
            withNeither = moveToPgn Standard mv pf

data PgnType = Standard | WithColumn | WithRow | WithBoth

moveToPgn :: PgnType -> Move -> PieceField -> PgnMove
moveToPgn Standard mv pf = moveToPgnHelper False False mv pf
moveToPgn WithColumn mv pf = moveToPgnHelper True False mv pf
moveToPgn WithRow mv pf = moveToPgnHelper False True mv pf
moveToPgn WithBoth mv pf = moveToPgnHelper True True mv pf

moveToPgnHelper :: Bool -> Bool -> Move -> PieceField -> PgnMove
moveToPgnHelper withColumn withRow mv pf = concat $ catMaybes values
  where
    pieceString = pgnPieceChar $ pfPiece pf
    columnString = fmap Ch.toLower $ shortColumn $ fieldColumn $ moveFrom mv
    rowString = shortRow $ fieldRow $ moveFrom mv
    targetString = fmap Ch.toLower $ shortField $ moveTo mv
    values = [Just pieceString, makeMaybe withColumn columnString, makeMaybe withRow rowString, Just targetString]

pgnPieceChar :: Piece -> String
pgnPieceChar piece = filter (not . (`elem` ("P"::String))) $ shortPiece piece

startGameFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
gs = parseOnly parseFen (Te.pack startGameFen)
startingGS = fromJust $ either (const Nothing) Just gs

mv = "1.e4 c5 2.Nc3 Nc6 3.f4 d6 4.Nf3 e6 5.Bc4 Nf6 6.d3 Be7 7.O-O O-O 8.Qe1 a6 9.a4 Qc7 10.Bd2 Rd8 11.e5 dxe5 12.fxe5 Nd5 13.Qg3 Nd4 14.Nxd4 Nxc3"
parsedEither = parseOnly parseGameMoves $ Te.pack mv
parsed = fromJust $ EitherC.rightToMaybe parsedEither
a = pgnAsMoves parsed
g = fromJust $ pgnGame parsed
moves = gameMoves g
gs' = fromJust $ tryMoves (Just startingGS) moves
mv' = fromJust $ pgnToMove gs' "bxc3"

-- s = move startingGS (Pawn, firstMove)
-- s' = move s (Pawn, secondMove)
-- thirdMove = fromJust $ stringToMove "E4D5"

-- pf = PieceField Pawn White (fromJust (stringToField "E4"))

eventParse :: Parser PgnTag = fmap PgnEvent $ tagParse "Event" $ many' $ letter <|> space <|> digit
siteParse :: Parser PgnTag = fmap PgnSite $ tagParse "Site" $ many' $ letter <|> space
dateParse :: Parser PgnTag = fmap PgnDate $ tagParse "Date" $ many' $ letter <|> space
roundParse :: Parser PgnTag = fmap (PgnRound . read) $ tagParse "Round" $ many' digit

tagParse :: String -> Parser a -> Parser a
tagParse tagName p = do
  string $ Te.pack $ "[" ++ tagName ++ " \""
  event <- p
  string "\"]"
  endOfLine
  return event

fullTagParse :: Parser PgnTag
fullTagParse = do
  result <- eventParse <|> siteParse <|> dateParse <|> roundParse <|> otherParse
  return result

parseAllTags :: Parser [PgnTag]
parseAllTags = do
  tags <- many' fullTagParse
  return tags

otherParse :: Parser PgnTag
otherParse = do
  char '['
  tagName <- many' letter
  space
  char '"'
  event :: Te.Text <- Data.Attoparsec.Text.takeWhile (\c -> c /='\"')
  string "\"]"
  endOfLine
  return $ PgnOther tagName (Te.unpack event)
  
data Player = Player {firstName :: String, lastName :: String} deriving (Show, Eq)

data PgnTag = 
    PgnEvent String
  | PgnSite String
  | PgnOther String String
  | PgnDate String
  | PgnRound Int
  | PgnWhite Player
  | PgnBlack Player
  | PgnResult PossibleResult
  deriving (Show, Eq)

data PossibleResult = WhiteWin | Draw | BlackWin deriving (Show, Eq)

pgnMoveMaybe :: Maybe GameState -> PgnMove -> Maybe (Piece, Move)
pgnMoveMaybe Nothing _ = Nothing
pgnMoveMaybe gs mp = pgnToMove (fromJust gs) mp

type FullState = (Maybe GameState, Maybe (Piece, Move))


pgnMoveFolder :: FullState -> PgnMove -> FullState
pgnMoveFolder (Nothing, _) _ = (Nothing, Nothing)
pgnMoveFolder (Just gs, _) pgnMove = (gs', mv)
  where gs' = fmap (move gs) mv
        mv = pgnToMove gs pgnMove

pgnAsMoves :: [PgnMove] -> [FullState]
pgnAsMoves pgnMoves = scanl pgnMoveFolder (Just startingGS, Nothing) pgnMoves

pgnGame :: [PgnMove] -> Maybe Game
pgnGame pgnMoves = liftA2 Game startingGS $ sequence moves
  where fs = pgnAsMoves pgnMoves
        startingGS = fst $ head fs
        movesWithPiece = tail $ fmap snd fs -- [Maybe (Piece, Move)]
        moves = ((fmap . fmap) snd) movesWithPiece -- [Maybe Move]

data Game = Game { startingGameState :: GameState, gameMoves :: [Move] } deriving Show
data PgnGame = PgnGame { pgnGameTags :: [PgnTag], parsedPgnGame :: Game } deriving Show

moveBegin = do
  many1' digit
  char '.'

bothMoveParser :: Parser [String]
bothMoveParser = do
  moveBegin
  moveWhite <- singleMoveParser
  space
  moveBlack <- singleMoveParser
  many' space
  many' endOfLine
  return [moveWhite, moveBlack]

whiteMoveParser :: Parser [String]
whiteMoveParser = do
  moveBegin
  moveWhite <- singleMoveParser
  many' space
  many' endOfLine
  return [moveWhite]

singleMoveParser :: Parser String
singleMoveParser = do
    first <- satisfy $ inClass "abcdefghKNBQRO"
    rest <- many1' (letter <|> digit <|> char '#' <|> char 'x' <|> char '+' <|> char '=' <|> char 'O' <|> char '-')
    return $ first : rest

moveParser = bothMoveParser <|> whiteMoveParser

filterMoves :: Char -> Bool
filterMoves c = not $ c `elem` ("x#+=" :: String)

parseGameMoves :: Parser [String]
parseGameMoves = do
  moves <- many1' moveParser
  return $ concat $ (fmap . fmap) (filter filterMoves) moves

parseWholeGame :: Parser (Maybe PgnGame)
parseWholeGame = do
  tags <- parseAllTags
  endOfLine
  moves <- parseGameMoves
  many' (space <|> digit <|> char '-' <|> char '/')
  many' endOfLine
  return $ uncurry (liftA2 PgnGame) (Just tags, pgnGame moves)

parseWholeGames :: Parser [Maybe PgnGame]
parseWholeGames = many1' parseWholeGame
