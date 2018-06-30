module Chess.Stockfish (mateFinder, MateMove, bestMoves, StockfishMove(..), sortMove, singleBestMove, evaluationNumber, resultLines, readResults, readMoves, Evaluation) where

import Chess.Board
import Chess.Logic
import Chess.Helpers

import Debug.Trace

import Control.Applicative (empty, (<|>))
import Control.Lens ((^.))
import Data.Attoparsec.Text (Parser, parseOnly, string, digit, char, space, letter)
import Data.Attoparsec.Combinator (many', choice)
import Data.Char (toUpper)
import Data.List (take, intercalate, sortOn)
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import Data.Text (Text, pack, splitOn, isInfixOf)
import Turtle (cd, shell, strict, input)
import Data.Either.Combinators (rightToMaybe)

type Fen = String
type MateMove = (Move, Int)

quoteString :: String -> String
quoteString s = "'" ++ s ++ "'"

-- Return a list of the best n moves
bestMoves :: Fen -> Int -> Int -> IO [StockfishMove]
bestMoves fen moveTime number = do
  cd "/home/cg/haskell-chess/scripts/"
  let arguments = fmap quoteString [fen, show moveTime, show number]
  let gs = fromJust $ fenToGameState fen
  let color = gs ^. gsColor
  let command = pack $ unwords $ "./bestmoves.sh" : arguments
  shell command empty
  moves <- readResults gs number
  let movesStandardized = if color == White then moves else fmap invertEval moves
  return movesStandardized

singleBestMove :: Fen -> Int -> Int -> IO (Maybe StockfishMove)
singleBestMove fen moveTime number = listToMaybe <$> bestMoves fen moveTime number


resultLines :: Int -> IO [Text]
resultLines number = do
  cd "/home/cg/haskell-chess/scripts/"
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
invertEval (StockfishMove m pv (Right num)) = StockfishMove m pv $ Right (-num)
invertEval (StockfishMove m pv e) = StockfishMove m pv e

sortMove :: StockfishMove -> Int
sortMove = evaluationNumber . sfEvaluation

evaluationNumber :: Evaluation -> Int
evaluationNumber (Left num) = 1000 - num
evaluationNumber (Right cp) = cp
    
getMates :: [StockfishMove] -> [MateMove]
getMates sfm = catMaybes $ fmap toMateMove sfm

mateFinder :: Fen -> IO [MateMove]
mateFinder f = do 
  moves <- bestMoves f 1000 5
  return $ getMates moves

toMateMove :: StockfishMove -> Maybe MateMove
toMateMove (StockfishMove mv _ (Left num)) = Just (mv, num)
toMateMove _ = Nothing

type MovePV = Int
type Evaluation = Either Int Int -- Left for mates, right for centipawns
data StockfishMove = StockfishMove {sfMove :: Move, sfMovePv :: Int, sfEvaluation :: Evaluation} deriving Show

pickMateMoves :: Either String StockfishMove -> Maybe (Move, Int)
pickMateMoves (Right (StockfishMove mv _ (Left num))) = Just (mv, num)
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
    many' digit
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
    return $ StockfishMove move pvNumber eval



