module Chess.Stockfish (mateFinder, MateMove, bestMoves, StockfishMove(..), sortMove, singleBestMove, evaluationNumber, resultLines, readResults, readMoves, Evaluation) where

import Chess.Board
import Chess.Logic
import Chess.Helpers

import Debug.Trace

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.Char as Ch
import Data.List
import Data.Maybe
import qualified Data.Text as Te
import qualified Data.ByteString as BS
import qualified Turtle as Tu
import qualified Data.Either.Combinators as EitherC

type Fen = String
type MateMove = (Move, Int)

quoteString :: String -> String
quoteString s = "'" ++ s ++ "'"

-- Return a list of the best n moves
bestMoves :: Fen -> Int -> Int -> IO [StockfishMove]
bestMoves fen moveTime number = do
  Tu.cd "/home/cg/haskell-chess/scripts/"
  let arguments = fmap quoteString [fen, show moveTime, show number]
  let gs = fromJust $ fenToGameState fen
  let color = gs ^. gsColor
  let command = Te.pack $ intercalate " " $ "./bestmoves.sh" : arguments
  Tu.shell command empty
  moves <- readResults gs number
  let movesStandardized = if color == White then moves else fmap invertEval moves
  return movesStandardized

singleBestMove :: Fen -> Int -> Int -> IO (Maybe StockfishMove)
singleBestMove fen moveTime number = fmap safeHead $ bestMoves fen moveTime number


resultLines :: Int -> IO [Te.Text]
resultLines number = do
  Tu.cd "/home/cg/haskell-chess/scripts/"
  results :: Te.Text <- Tu.strict $ Tu.input "./results.txt"
  let lines =  Te.splitOn "\n" results
  let filt = \t -> not $ (Te.pack "upperbound") `Te.isInfixOf` t
  return $ filter filt lines

readResults :: GameState -> Int -> IO [StockfishMove]
readResults gs number = do
  lines <- resultLines number
  return $ readMoves gs lines number

readMoves :: GameState -> [Te.Text] -> Int -> [StockfishMove]
readMoves gs t number = reverse $ sortOn sortMove $ catMaybes $ fmap (EitherC.rightToMaybe . parser) $ lastLines
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
stockfishMoveRead gs mv = snd $ fromJust $ stringToMove gs $ fmap Ch.toUpper mv

stockfishLineParser :: GameState -> Parser StockfishMove
stockfishLineParser gs = do
    string "info depth "
    many' digit
    string " seldepth "
    many' digit
    string " multipv "
    pvString <- many' digit
    let pvNumber = (read pvString) :: Int
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

