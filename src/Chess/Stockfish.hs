module Chess.Stockfish (
  mateFinder, MateMove, bestMoves, FullMoveStats(..), StockfishMove(..), sortMove, singleBestMove, evaluationNumber, resultLines, readResults, readMoves, Evaluation) where

import Chess.Board
import Chess.Logic
import Chess.Helpers

import Debug.Trace

import Control.Applicative (liftA2, empty, (<|>))
import Control.Lens ((^.))
import Data.Attoparsec.Text (Parser, parseOnly, string, digit, char, space, letter)
import Data.Attoparsec.Combinator (many', choice)
import Data.Char (toUpper)
import Data.List (take, intercalate, sortOn, sortBy)
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import Data.Text (Text, pack, splitOn, isInfixOf)
import Turtle (cd, shell, strict, input)
import Data.Either.Combinators (rightToMaybe)
import System.FilePath (FilePath)

type Fen = String
type MateMove = (Move, Int)

quoteString :: String -> String
quoteString s = "'" ++ s ++ "'"

scriptsFolder = "/home/cg/code/haskell-chess/scripts/"

-- Return a list of the best n moves
bestMoves :: Fen -> Int -> Int -> Int -> IO [StockfishMove]
bestMoves fen moveTime numberPV numberDepths = do
  cd scriptsFolder
  let arguments = fmap quoteString [fen, show moveTime, show numberPV]
  let gs = fromJust $ fenToGameState fen
  let color = gs ^. gsColor
  let command = pack $ unwords $ "./bestmoves.sh" : arguments
  shell command empty
  moves <- readResults gs (numberPV * numberDepths)
  let movesStandardized = if color == White then moves else fmap invertEval moves
  return $ sortOn sfDepth $ movesStandardized

numberDepths = 10

complexityGuidBratko :: [StockfishMove] -> Int
complexityGuidBratko mvs = absChanges
  where
    withLagged = zip (tail mvs) (init mvs)
    doesChange (first, second) = sfMove first /= sfMove second
    onlyChanged = filter doesChange withLagged
    evalNumber = evaluationNumber . sfEvaluation
    changedEval (first, second) = abs $ evalNumber first - evalNumber second
    absChanges = sum $ fmap changedEval onlyChanged


data FullMoveStats = FullMoveStats {
  msSf :: StockfishMove
, msComplexityGuidBratko :: Int
} deriving (Show)


singleBestMove :: Fen -> Int -> IO (Maybe FullMoveStats)
singleBestMove fen moveTime = do
  best <- bestMoves fen moveTime 1 numberDepths
  let complexityGB = complexityGuidBratko best
  let bestMove = listToMaybe $ reverse $ best
  return $ (liftA2 FullMoveStats) bestMove (Just complexityGB)

resultLines :: Int -> IO [Text]
resultLines number = do
  cd scriptsFolder
  results :: Text <- strict $ input "./results.txt"
  let lines =  splitOn "\n" results
  let filt t = not $ pack "upperbound" `isInfixOf` t
  return $ filter filt lines

readResults :: GameState -> Int -> IO [StockfishMove]
readResults gs number = do
  lines <- resultLines number
  return $ readMoves gs lines number

readMoves :: GameState -> [Text] -> Int -> [StockfishMove]
readMoves gs t number = reverse $ sortOn sortMove $ catMaybes $ (rightToMaybe . parser) <$> lastLines
  where   lastLines = Data.List.take number $ tail . tail $ reverse t 
          parser = parseOnly (stockfishLineParser gs)


-- A sort order that ensures that mates are better than any non-mate
invertEval :: StockfishMove -> StockfishMove
invertEval (StockfishMove m pv d (Right num)) = StockfishMove m pv d $ Right (-num)
invertEval (StockfishMove m pv d e) = StockfishMove m pv d e

sortMove :: StockfishMove -> Int
sortMove = evaluationNumber . sfEvaluation

evaluationNumber :: Evaluation -> Int
evaluationNumber (Left num) = 1000 - num
evaluationNumber (Right cp) = cp
    
getMates :: [StockfishMove] -> [MateMove]
getMates sfm = sortOn snd $ catMaybes $ fmap toMateMove sfm

mateFinder :: Fen -> IO [MateMove]
mateFinder f = do 
  moves <- bestMoves f 1000 5 1
  return $ getMates moves

toMateMove :: StockfishMove -> Maybe MateMove
toMateMove (StockfishMove mv _ _ (Left num)) = Just (mv, num)
toMateMove _ = Nothing

type MovePV = Int
type Evaluation = Either Int Int -- Left for mates, right for centipawns
data StockfishMove = StockfishMove {
    sfMove :: Move
  , sfMovePv :: Int
  , sfDepth :: Int
  , sfEvaluation :: Evaluation
} deriving Show

pickMateMoves :: Either String StockfishMove -> Maybe (Move, Int)
pickMateMoves (Right (StockfishMove mv _ _ (Left num))) = Just (mv, num)
pickMateMoves _ = Nothing

mateParser :: Parser Evaluation
mateParser = fmap (Left . read) $ string "mate " >> many' digit

centipawnParser :: Parser Evaluation
centipawnParser = fmap (Right . read) $ do
  string "cp " 
  minus <- many' $ char '-'
  digits <- many' digit
  return $ minus ++ digits

parseHash :: Parser ()
parseHash = do
  string "hashfull "
  many' digit
  space
  return ()

stockfishMoveRead :: GameState -> String -> Move
stockfishMoveRead gs mv = snd $ fromJust $ stringToMove gs $ fmap toUpper mv

stockfishLineParser :: GameState -> Parser StockfishMove
stockfishLineParser gs = do
    string "info depth "
    depth <- many' digit
    let depthInt = read depth :: Int
    string " seldepth "
    many' digit
    string " multipv "
    pvString <- many' digit
    let pvNumber = read pvString :: Int
    string " score " 
    eval <- choice [mateParser, centipawnParser]
    string " nodes "
    many' digit
    string " nps "
    many' digit
    space
    many'  parseHash
    string "tbhits "
    many' digit
    string " time "
    many' digit
    string " pv "
    mvString <- many' (letter <|> digit)
    many' (letter <|> digit <|> space <|> char '\r')
    let move = stockfishMoveRead gs mvString
    return $ StockfishMove move pvNumber depthInt eval



