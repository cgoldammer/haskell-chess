{-# LANGUAGE OverloadedStrings, FlexibleInstances, OverlappingInstances, ScopedTypeVariables #-}

module Lib where

import Data.List
import qualified Data.Set as S
import Control.Monad
import qualified Data.Char as Ch
import qualified Control.Lens as L
import Data.Maybe

import qualified Data.Tuple as Tu
import Text.Read
import qualified Data.Text as Te
import qualified Data.ByteString.Lazy as La
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Char (chr)

import System.Random

import Algorithms
import Board
import Logic
import Various
import Control.Applicative

-- import Data.Attoparsec
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.Char8 as C
import qualified Turtle as Tu
import Data.Aeson

fen = fullFen mate1PS

runChess :: IO ()
runChess = do
    rGood <- randomGood 20000
    print $ rGood
    print $ length rGood
    let positions = fmap gsPosition rGood
    let fens = fmap fullFen positions
    mates :: [[MateMove]] <- traverse mateFinder fens
    let both = zip positions mates
    let good = filter (\(ps, m) -> length m > 0) both
    let g2 = filter (\(ps, m) -> filterMates m) good
    let printable = fmap (\(num, (ps, m)) -> PositionMates num ps m) $ zip [0..] g2
    print $ encode printable
    let enc = (encode printable) :: La.ByteString
    let s = U.toString enc
    -- Tu.output "/home/cg/haskell/chess/js/mates.json" "hi"
    writeFile "/home/cg/haskell/projects/chess/js/mates.json" s
    return ()

-- Todo;
-- Export g2 as json, store in file
-- Read in react

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
filterMates mm = numberMin == 1 && minNumber <= 3 && minNumber > 0
    where   minNumber = minimum numbers
            numbers = fmap snd mm
            numberMin = length $ filter (minNumber==) numbers

type MateMove = (Move, Int)

type Fen = String

mateFinder :: Fen -> IO [MateMove]
mateFinder f = do
    Tu.cd "/home/cg/haskell/projects/chess/stock/tests"
    Tu.shell (Te.pack $ "./bestmoves.sh \"" ++ f ++ "\"") empty
    moves <- readResults
    return moves

readResults :: IO [MateMove]
readResults = do
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

testParse :: Parser (String)
testParse = do
    string "info depth "
    pvNumber <- many' digit
    return (pvNumber)



-- Todo:
-- Algorithmic: 
--  - Restrict to interesting positions
--   - only one queen
--   - no more than two rooks, knights, or bishops
--   - not two bishops of same color
--   - black King in the middle
--   - first move isn't check
--  - Mates in 2, 3, 4. In UI, quiz for #
-- 
-- Search over actual games: 
--  - Find all positions that have a mate in 2, 3, or 4. Quiz: # of moves to mate
--  - Restrict to positions in which the player didn't find the mate
--
--


-- stockFishParse :: T.Text -> [MateMove]
-- stockFishParse = undefined
-- select latest Multi lines
-- drop the bestmove line
