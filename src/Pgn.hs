{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}

module Pgn (startGameFen) where

import Board
import Logic

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

pgnToMove :: GameState -> PgnMove -> Move
pgnToMove = undefined

startGameFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"




pgnGame :: [pgnMove] -> Game
pgnGame = undefined
-- pgnGame pgnMoves = foldl move startGameState pgnMoves

data Game = Game { startingGameState :: GameState, gameMoves :: [Move] }


-- In other words, IO gives me [Game]. Then use it.



