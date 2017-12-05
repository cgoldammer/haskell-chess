module Chess.Lib (runChess) where

import Data.List
import qualified Data.Set as S
import Control.Monad
import Control.Lens hiding ((.=))
import qualified Control.Lens as L
import Data.Maybe
import Options.Applicative -- iding ((<>))
-- import Data.Semigroup hiding ((<>))

import Text.Read
import qualified Data.Text as Te
import qualified Data.ByteString.Lazy as La
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Char (chr)

import System.Random

import Chess.Algorithms
import Chess.Stockfish
import Chess.Board
import Chess.Logic
import Chess.Pgn.Logic as Pgn

import Control.Applicative

import Data.Aeson
import qualified Turtle as Tu

-- fen = fullFen mate1PS

mateFromGameState :: GameState -> IO [MateMove]
mateFromGameState = mateFinder . fullFen . (view gsPosition)

-- resultsString :: [GameState] -> [[MateMove]] -> String
-- resultsString gs mates = U.toString $ encode printable
--     where   goodMateMoves = filter (\(ps, m) -> filterMates m) (zip (fmap (view gsPosition) gs) mates)
--             printable = fmap (\(num, (ps, m)) -> PositionMates num ps m) $ zip [0..] goodMateMoves

data PositionMates = PositionMates Int Position [MateMove]

instance ToJSON PositionMates where
    toJSON (PositionMates num ps mm) = object [
        "id" .= num,
        "fen" .= fullFen ps,
        "mate" .= minimum (fmap snd mm),
        "best" .= show (bestMove mm (minimum (fmap snd mm)))]
    
bestMove :: [MateMove] -> Int -> Move
bestMove mm num = fst $ head $ filter (\m -> ((snd m) == num)) mm

-- filterMates :: [MateMove] -> Bool
-- filterMates mm = length mm > 0 && numberMin == 1 && minNumber >= 1 && minNumber <= 5
--     where   minNumber = minimum numbers
--             numbers = fmap snd mm
--             numberMin = length $ filter (minNumber==) numbers


data ParseInput = ParseInput { 
    fileName :: FilePath
  , startMove :: Int
  , endMove :: Int
  , gameStart :: Int
  , gameEnd :: Int }

data Input = FileInput FilePath | StdInput

fileInput :: Parser FilePath
fileInput = strOption
  ( long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input file")

-- stdInput :: Parser Input
-- stdInput = flag' StdInput
--   (  long "stdin"
--   <> help "Read from stdin")

-- input :: Parser Input
-- input = fileInput <|> stdInput

runChess :: IO ()
runChess = greet =<< execParser opts
  where
    opts = info (parse <**> helper)
      ( fullDesc
      <> progDesc "Print a greeting"
      <> header "A header")

greet :: ParseInput -> IO ()
greet (ParseInput fn sm _ _ ge) = do
  let strings = [""]
  mapM_ putStrLn strings
  return ()
      
parse :: Parser ParseInput
parse = ParseInput
  <$> fileInput 
  <*> option auto
    ( long "startMove"
      <> help "Starting move for evaluation"
      <> showDefault
      <> value 1
      <> metavar "INT")
  <*> option auto
    ( long "endMove"
      <> help "Env move for evaluation"
      <> showDefault
      <> value 2
      <> metavar "INT")
  <*> option auto
    ( long "gameStart"
      <> help "Starting game for evaluation"
      <> showDefault
      <> value 1
      <> metavar "INT")
  <*> option auto
    ( long "gameEnd"
      <> help "Ending game for evaluation"
      <> showDefault
      <> value 1
      <> metavar "INT")

