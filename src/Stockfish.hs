{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
module Stockfish (mateFinder, MateMove) where

import Board
import Logic

import Control.Applicative
import Data.Aeson
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.Char8 as C
import qualified Data.Char as Ch
import Data.List
import Data.Maybe
import qualified Data.Text as Te
import qualified Turtle as Tu

type Fen = String
type MateMove = (Move, Int)

mateFinder :: Fen -> IO [MateMove]
mateFinder f = do
    Tu.cd "/home/cg/haskell-chess/scripts/"
    Tu.shell (Te.pack $ "./bestmoves.sh \"" ++ f ++ "\"") empty
    moves <- readResults
    return moves

readResults :: IO [MateMove]
readResults = do
    Tu.cd "/home/cg/haskell-chess/scripts/"
    results :: Te.Text <- Tu.strict $ Tu.input "./results.txt"
    let mates = getMates (Te.splitOn "\n" results)
    return mates
    
getMates :: [Te.Text] -> [MateMove]
getMates t = catMaybes $ fmap (pickMateMoves . parser) lastLines
    where   lastLines = Data.List.take 7 $ reverse t 
            parser = parseOnly lineParse

pickMateMoves (Right (pv, num , [fR, fC, tR, tC])) = Just (Move from to Nothing, num)
    where   from = fromJust $ stringToField ([fR, fC])
            to = fromJust $ stringToField ([tR, tC])
pickMateMoves (Left _) = Nothing
pickMateMoves (Right (pv, num , _)) = Nothing

lineParse :: Parser (Int, Int, String)
lineParse = do
    string "info depth "
    many' digit
    string " seldepth "
    many' digit
    string " multipv "
    pvNumber <- many' digit
    string " score mate "
    numMoves <- many' digit
    string " nodes "
    many' digit
    string " nps "
    many' digit
    string " tbhits "
    many' digit
    string " time "
    many' digit
    string " pv "
    mv <- many' (letter <|> digit)
    many' (letter <|> digit <|> space)
    return (read pvNumber, read numMoves, fmap Ch.toUpper mv)
