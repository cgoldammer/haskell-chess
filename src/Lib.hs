{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}

module Lib where

import Data.List
import qualified Data.Set as S
import Control.Monad
import qualified Control.Lens as L
import Data.Maybe

import Text.Read
import qualified Data.Text as Te
import qualified Data.ByteString.Lazy as La
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Char (chr)

import System.Random

import Algorithms
import Stockfish
import Board
import Logic
import Various
import Control.Applicative

import Data.Aeson
import qualified Turtle as Tu


fen = fullFen mate1PS

mateFromGameState :: GameState -> IO [MateMove]
mateFromGameState = mateFinder . fullFen . gsPosition

resultsString :: [GameState] -> [[MateMove]] -> String
resultsString gs mates = U.toString $ encode printable
    where   goodMateMoves = filter (\(ps, m) -> filterMates m) (zip (fmap gsPosition gs) mates)
            printable = fmap (\(num, (ps, m)) -> PositionMates num ps m) $ zip [0..] goodMateMoves

runChess :: IO ()
runChess = do
    randomStates :: [GameState] <- randomGood 5000
    print $ length randomStates
    mates :: [[MateMove]] <- traverse mateFromGameState randomStates 
    let printable = resultsString randomStates mates
    print printable
    writeFile "/home/cg/data/output/mates.json" printable
    return ()

data PositionMates = PositionMates Int Position [MateMove]

instance ToJSON PositionMates where
    toJSON (PositionMates num ps mm) = object [
        "id" .= num,
        "fen" .= fullFen ps,
        "mate" .= minimum (fmap snd mm),
        "best" .= show (bestMove mm (minimum (fmap snd mm)))]
    
bestMove :: [MateMove] -> Int -> Move
bestMove mm num = fst $ head $ filter (\m -> ((snd m) == num)) mm

filterMates :: [MateMove] -> Bool
filterMates mm = length mm > 0 && numberMin == 1 && minNumber >= 1 && minNumber <= 5
    where   minNumber = minimum numbers
            numbers = fmap snd mm
            numberMin = length $ filter (minNumber==) numbers


