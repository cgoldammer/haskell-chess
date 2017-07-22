{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}

module Pgn (
    startGameFen
  , pgnToMove
  , PgnType (Standard, WithColumn, WithRow, WithBoth)
  , possibleMoveFields
  , pgnPiece
  , pgnToTargetField
  , PgnTag (..)
  , tagParse
  , eventParse, siteParse, fullTagParse
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

import Board
import Logic
import Various

type PgnMove = String

pgnMove :: GameState -> Move -> PgnMove
pgnMove = undefined

possibleMoveFields :: GameState -> PgnMove -> [(Move, [String])]
possibleMoveFields gs pgn = zip moves pgnMoves
    where   movePiece = pgnPiece pgn
            allMoves = filter (\(piece, _) -> piece == movePiece) $ allNextLegalMoves gs
            targetField = pgnToTargetField pgn
            color = gsColor gs
            movesWithPieceFields = [(mv, PieceField piece color (moveFrom mv)) | (piece, mv) <- allMoves, targetField == Just (moveTo mv)]
            moves = fmap fst movesWithPieceFields
            pgnMoves = createPgnMoves gs movesWithPieceFields

pgnToTargetField pgn = stringToField $ fmap Ch.toUpper $ reverse $ Data.List.take 2 $ reverse pgn

pgnToMove :: GameState -> PgnMove -> Maybe Move
pgnToMove gs pgn = listToMaybe [mv | (mv, pgnList) <- possibleMoveFields gs pgn, pgn `elem` pgnList]

pgnPiece :: PgnMove -> Piece
pgnPiece pgnMove
  | head pgnMove `elem` ("KQRNB" :: String) = fromJust $ stringToPiece $ [head pgnMove]
  | otherwise = Pawn



createPgnMoves :: GameState -> [(Move, PieceField)] -> [[PgnMove]]
createPgnMoves gs ls
    | length ls == 1 = [[moveToPgn Standard (fst (head ls)) (snd (head ls))]]
    | otherwise = fmap (\el -> expandMove gs (fst el) (snd el)) ls


expandMove :: GameState -> Move -> PieceField -> [PgnMove]
expandMove gs mv pf = [withColumn, withRow, withBoth]
    where   withColumn = moveToPgn WithColumn mv pf
            withRow = moveToPgn WithRow mv pf
            withBoth = moveToPgn WithBoth mv pf
        

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

-- data ParsedGame = ParsedGame { startingPosition :: GameState, moves :: [Move]}
-- data PgnGame = { PgnTags :: [PgnTag], PgnGame :: ParsedGame }

-- pgnGameParse :: Parser PgnGame
--   tags <- many' $ eventParse <|> siteParse <|> otherParse
--   emptyline
--   game :: String <- many' moveSetParser

--   return $ tags game

-- gameParse :: Parser String
-- gameParse = do
--   results <- many' (
--   results <- many' (char <|> space <|> digit

-- moveSetParser :: Parser String
-- moveSetParser = do
--   number <- digit
--   char '.'
--   space
--   moves <- many' (digit <|> char <|> space)
--   return

-- -- Fields: Event, Site, Date, Round, White, Black, Result, WhiteElo, BlackElo, ECO"


eventParse :: Parser PgnTag = fmap PgnEvent $ tagParse "Event" $ many' $ letter <|> space <|> digit
siteParse :: Parser PgnTag = fmap PgnSite $ tagParse "Site" $ many' $ letter <|> space
dateParse :: Parser PgnTag = fmap PgnDate $ tagParse "Date" $ many' $ letter <|> space
roundParse :: Parser PgnTag = fmap (PgnRound . read) $ tagParse "Round" $ many' digit

tagParse :: String -> Parser a -> Parser a
tagParse tagName p = do
  string $ Te.pack $ "[" ++ tagName ++ " \""
  event <- p
  string "\"]"
  return event

fullTagParse :: Parser PgnTag
fullTagParse = do
  result <- eventParse <|> siteParse <|> dateParse <|> roundParse <|> otherParse
  return result

otherParse :: Parser PgnTag
otherParse = do
  char '['
  tagName <- many' letter
  space
  char '"'
  event :: String <- many' $ letter <|> digit <|> space
  string "\"]"
  return $ PgnOther tagName event
  
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



pgnGame :: [pgnMove] -> Game
pgnGame = undefined
-- pgnGame pgnMoves = foldl move startGameState pgnMoves

data Game = Game { startingGameState :: GameState, gameMoves :: [Move] }


-- In other words, IO gives me [Game]. Then use it.




