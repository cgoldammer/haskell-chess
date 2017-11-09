{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables, DeriveGeneric #-}

module Chess.Pgn (
    startGameFen
  , pgnToMove, cleanPgn, pgnAsMoves
  , PgnType (Standard, WithColumn, WithRow, WithBoth)
  , possibleMoveFields
  , pgnPiece
  , pgnToTargetField, pgnToPromotion
  , PgnTag (..), Player (..)
  , PgnGame (..)
  , tagParse
  , eventParse, siteParse, fullTagParse, parseAllTags
  , parseGameMoves, moveParser, bothMoveParser, sidelineParser
  , readSingleGame
  , pgnGame
  , gameBlunders
  , gameText
  , BlunderThreshold
  , GameEvaluation(..)
  , startingGS
  , getGames
  , moveToPgn) where

import Debug.Trace

import Chess.Board
import Chess.Logic
import Chess.Helpers
import Chess.Stockfish

import GHC.Generics

import Control.Lens hiding ((.=))
import Control.Applicative
import qualified Data.ByteString.Lazy.UTF8 as U
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
import qualified Turtle as Tu
import qualified Filesystem.Path.CurrentOS as FS

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
            color = gs ^. gsColor
            movesWithPieceFields = [(mv, PieceField piece color (mv ^. moveFrom)) | (piece, mv) <- allMoves, targetField == Just (mv ^. moveTo) && promotionPiece == mv ^. movePromotionPiece]
            movesWithPiece = [(pf ^. pfPiece, m) | (m, pf) <- movesWithPieceFields]
            pgnMoves = createPgnMoves gs movesWithPieceFields

pgnToTargetField pgn = stringToField $ fmap Ch.toUpper $ reverse $ Data.List.take 2 $ reverse pgn

pgnToMove :: GameState -> PgnMove -> Maybe (Piece, Move)
pgnToMove gs pgn = listToMaybe [mv | (mv, pgnList) <- possibleMoveFields gs (pgnParse pgn color), pgnCleaned `elem` pgnList]
    where color = gs ^. gsColor
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
    pieceString = pgnPieceChar $ pf ^. pfPiece
    columnString = fmap Ch.toLower $ shortColumn $ (mv ^. moveFrom) ^. fieldColumn
    rowString = shortRow $ mv ^. moveFrom ^. fieldRow
    targetString = fmap Ch.toLower $ shortField $ mv ^. moveTo
    values = [Just pieceString, makeMaybe withColumn columnString, makeMaybe withRow rowString, Just targetString]

pgnPieceChar :: Piece -> String
pgnPieceChar piece = filter (not . (`elem` ("P"::String))) $ shortPiece piece

startGameFen = "fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
gs = parseOnly parseFen (Te.pack startGameFen)
startingGS = fromJust $ either (const Nothing) Just gs

-- mv = "1.e4 c5 2.Nc3 Nc6 3.f4 d6 4.Nf3 e6 5.Bc4 Nf6 6.d3 Be7 7.O-O O-O 8.Qe1 a6 9.a4 Qc7 10.Bd2 Rd8 11.e5 dxe5 12.fxe5 Nd5 13.Qg3 Nd4 14.Nxd4 Nxc3"
-- parsedEither = parseOnly parseGameMoves $ Te.pack mv
-- parsed = fromJust $ EitherC.rightToMaybe parsedEither
-- a = pgnAsMoves parsed
-- g = fromJust $ pgnGame parsed
-- moves = gameMoves g
-- gs' = fromJust $ tryMoves (Just startingGS) moves
-- mv' = fromJust $ pgnToMove gs' "bxc3"

-- s = move startingGS (Pawn, firstMove)
-- s' = move s (Pawn, secondMove)
-- thirdMove = fromJust $ stringToMove "E4D5"

-- pf = PieceField Pawn White (fromJust (stringToField "E4"))

eventParse :: Parser PgnTag = fmap PgnEvent $ tagParse "Event" $ many' $ letter <|> space <|> digit <|> char '?'
siteParse :: Parser PgnTag = fmap PgnSite $ tagParse "Site" $ many' $ letter <|> space
dateParse :: Parser PgnTag = fmap PgnDate $ tagParse "Date" $ many' $ letter <|> space
roundParse :: Parser PgnTag = fmap (PgnRound . read) $ tagParse "Round" $ many' digit
whitePlayerParse :: Parser PgnTag = fmap PgnWhite $ tagParse "White" $ nameParser
blackPlayerParse :: Parser PgnTag = fmap PgnBlack $ tagParse "Black" $ nameParser

nameParser :: Parser Player
nameParser = do
  last <- many' letter
  many' (space <|> char ',')
  first <- many' letter
  return $ Player first last

nameParse = letter <|> char ',' <|> space

tagParse :: String -> Parser a -> Parser a
tagParse tagName p = do
  string $ Te.pack $ "[" ++ tagName ++ " \""
  event <- p
  string "\"]"
  endOfLine
  return event

fullTagParse :: Parser PgnTag
fullTagParse = do
  result <- eventParse <|> siteParse <|> dateParse <|> roundParse <|> whitePlayerParse <|> blackPlayerParse <|> otherParse
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
  
data Player = Player {firstName :: String, lastName :: String} deriving (Eq)

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

formatForDB :: PgnTag -> (String, String)
formatForDB (PgnEvent s) = ("Event", s)
formatForDB (PgnOther name s) = (name, s)
formatForDB (PgnDate s) = ("Date", s)
formatForDB (PgnRound s) = ("Round", show s)
formatForDB (PgnWhite player) = ("White", show player)
formatForDB (PgnBlack player) = ("White", show player)

data PossibleResult = WhiteWin | Draw | BlackWin deriving (Eq)

instance Show Player where
  show (Player first last) = first ++ " " ++ last

instance Show PossibleResult where
  show WhiteWin = "1"
  show BlackWin = "0"
  show Draw = "D"

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

foldGameEither :: Either String [Move] -> Maybe Move -> Either String [Move] 
foldGameEither (Right mvs) Nothing = Left $ "Parsed moves until " ++ show mvs
foldGameEither (Right mvs) (Just mv) = Right $ mvs ++ [mv]
foldGameEither (Left err) _ = Left err

data PgnGame = PgnGame { 
    pgnGameTags :: [PgnTag]
  , parsedPgnGame :: Game
  , pgnMoves :: [String] } deriving (Show)

data Game = Game { startingGameState :: GameState, gameMoves :: [Move] } deriving (Show)

moveBegin :: Parser ()
moveBegin = do
  many1' digit
  many' $ char '.'
  return ()

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
  many' space
  moveWhite <- singleMoveParser
  many' space
  many' endOfLine
  return [moveWhite]

commentParser :: Parser ()
commentParser = do
  many' space
  char '{'
  skipWhile (/='}')
  char '}'
  return ()
    
dollarParser :: Parser ()
dollarParser = do
  many' space
  char '$'
  many' digit
  return ()

-- ignoreInteriorSidelines :: Parser ()
-- ignoreInteriorSidelines = do
--   skipWhile $ \c -> not (c `elem` ("(" :: String))
--   many' sidelineParser
--   return ()
--

sidelineParser :: Parser ()
sidelineParser = do
  many' space
  char '('
  -- takeTill (\c `elem` "()")
  takeTill (\c -> c `elem` ("()"::String))
  many' sidelineParser
  takeTill (\c -> c `elem` ("()"::String))
  many' sidelineParser
  takeTill (\c -> c `elem` ("()"::String))
  many' sidelineParser
  takeTill (\c -> c `elem` ("()"::String))
  many' sidelineParser
  takeTill (\c -> c `elem` ("()"::String))
  many' sidelineParser
  takeTill (\c -> c `elem` ("()"::String))
  many' sidelineParser
  takeTill (\c -> c `elem` ("()"::String))
  many' sidelineParser
  takeTill (\c -> c `elem` ("()"::String))
  many' sidelineParser
  takeTill (\c -> c `elem` ("()"::String))
  char ')'
  return ()

singleMoveParser :: Parser String
singleMoveParser = do
  many' space
  first <- satisfy $ inClass "abcdefghKNBQRO"
  rest <- many1' (letter <|> digit <|> char '#' <|> char 'x' <|> char '+' <|> char '=' <|> char 'O' <|> char '-')
  many' dollarParser
  many' $ sidelineParser <|> commentParser
  return $ first : rest

moveParser = bothMoveParser <|> whiteMoveParser

filterMoves :: Char -> Bool
filterMoves c = not $ c `elem` ("x#+=" :: String)

parseGameMoves :: Parser [String]
parseGameMoves = do
  many' $ sidelineParser <|> commentParser
  many' space
  moves <- many1' moveParser
  return $ concat $ (fmap . fmap) (filter filterMoves) moves


data GameData = GameData [PgnTag] [String]


parseGameComponents :: Parser GameData
parseGameComponents = do
  tags <- parseAllTags
  many' endOfLine
  moves <- parseGameMoves
  many' (space <|> digit <|> char '-' <|> char '/' <|> char '*')
  many' endOfLine
  return $ GameData tags moves

composeGame :: Either String GameData -> ParsedGame
composeGame (Left s) = Left $ AttoParseError s
composeGame (Right (GameData gt pgn)) = parseGame gt pgn

gameText :: String -> Int -> IO [Te.Text]
gameText fp num = fmap ((Data.List.take num) . splitIntoGames) $ Tu.strict $ Tu.input $ FS.fromText $ Te.pack fp

splitIntoGames :: Te.Text -> [Te.Text]
splitIntoGames text = fmap Te.concat [[needle, t] | t <- tail (Te.splitOn needle text)]
  where needle = Te.pack "[Event "

data ParseError = AttoParseError String | PgnParseError String deriving (Show)
type ParsedGame = Either ParseError PgnGame

readSingleGame :: Te.Text -> ParsedGame
readSingleGame text = composeGame $ parseOnly parseGameComponents text

pgnGame :: [PgnMove] -> Either String Game
pgnGame pgnMoves = fmap (Game startingGS) eitherMoves
  where fs = pgnAsMoves pgnMoves
        startingGS = fromJust $ fst $ head fs
        movesWithPiece = tail $ fmap snd fs -- [Maybe (Piece, Move)]
        moves = ((fmap . fmap) snd) movesWithPiece -- [Maybe Move]
        eitherMoves = foldl foldGameEither (Right []) moves

parseGame :: [PgnTag] -> [PgnMove] -> ParsedGame
parseGame tags moves = gameParser game
  where game = pgnGame moves
        gameParser (Left s) = Left $ PgnParseError s
        gameParser (Right g) = Right $ PgnGame tags g moves
  
getGames :: String -> Int -> IO [ParsedGame]
getGames fp num = do
  gamePgn :: [Te.Text] <- gameText fp num
  return $ fmap readSingleGame gamePgn

interestingRange :: Int -> (Int, Int)
interestingRange num = (start, end)
  where middle = quot num 2
        start = min 30 middle
        end = min (start + 20) num

slice from to xs = Data.List.take (to - from + 1) (Data.List.drop from xs)

data GameEvaluation = GameEvaluation { gameTags :: [PgnTag], gameMoveSummaries :: [MoveSummary] } deriving (Show, Generic)

filterForBlunders :: [MoveSummary] -> Int -> [MoveSummary]
filterForBlunders = undefined

gameSummaries :: PgnGame -> IO [(MoveSummary, GameState)]
gameSummaries gm = do 
  let g = parsedPgnGame gm
  let moves = gameMoves g
  let gameStates = scanl move' (startingGameState g) moves
  bestMoves <- best $ zip gameStates moves
  return $ formatBest $ concurrentJust $ bestMoves
  
type BlunderThreshold = Int

gameBlunders :: PgnGame -> BlunderThreshold -> IO GameEvaluation
gameBlunders g bt = do
  summaries <- gameSummaries g
  let blunders = filter (\ms -> msComparison ms <= (- bt)) $ fmap fst summaries
  let gt = pgnGameTags g
  return $ GameEvaluation gt blunders

gameCSV :: PgnGame -> IO String
gameCSV gm = do 
  let gt = pgnGameTags gm
  let g = parsedPgnGame gm
  let moves = gameMoves g
  let start = startingGameState g
  let gameStates = scanl move' start moves
  mv <- best $ zip gameStates moves
  return $ gameSummary gt mv

best :: [(GameState, Move)] -> IO [(Move, Maybe StockfishMove, GameState)]
best positionsRaw = do
  let numberMoves = length positionsRaw
  let (start, end) = interestingRange numberMoves
  let (start, end) = (0, numberMoves)
  let positions = slice start end positionsRaw
  let mvs = fmap snd positions
  let states = fmap fst positions
  bests <- mapM (\gs -> singleBestMove (gameStateToFen gs) 100 1) states
  return $ zip3 mvs bests states

gameSummary :: [PgnTag] -> [(Move, Maybe StockfishMove, GameState)] -> String
gameSummary gt summ = tagSummaries ++ "," ++ evalSummaries
  where evals = gameEvaluations summ
        (whiteEval, blackEval) = evalSummary evals
        tagSummaries = Data.List.intercalate "," $ catMaybes $ fmap tagSummary gt
        evalSummaries = show (evalAverage whiteEval) ++ "," ++ show (evalAverage blackEval)

tagSummary :: PgnTag -> Maybe String
tagSummary (PgnWhite player) = Just $ lastName player
tagSummary (PgnBlack player) = Just $ lastName player
tagSummary _ = Nothing

data PlayerEval = PlayerEval { evalAverage :: Int } deriving Show

type GameEval = (PlayerEval, PlayerEval)

evalSummary :: [(Color, Int)] -> GameEval
evalSummary evals = (evalWhite, evalBlack)
  where evalWhite = toPlayerEval $ fmap snd $ (filter (\e -> fst e == White)) evals
        evalBlack = toPlayerEval $ fmap snd $ (filter (\e -> fst e == Black)) evals

average xs = realToFrac (sum xs) / genericLength xs

toPlayerEval :: [Int] -> PlayerEval
toPlayerEval evals = PlayerEval $ round $ average evals

concurrentJust :: [(Move, Maybe StockfishMove, GameState)] -> [(Move, StockfishMove, GameState)]
-- concurrentJust (m, Just sfm, gs): (m', Just sfm', gs'): rest = (m, sfm, gs): (m', sfm', gs'): concurrentJust rest
concurrentJust _ = []

gameEvaluations :: [(Move, Maybe StockfishMove, GameState)] -> [(Color, Int)]
gameEvaluations summ = results
  where formatted = formatBest summWithSF
        summWithSF = [(m, fromJust sfm, gs) | (m, sfm, gs) <- summ, isJust sfm]
        results = [(gs ^. gsColor, msComparison ms) | (ms, gs) <- formatted]
        
data MoveSummary = MoveSummary {msMove :: Move, msMoveBest :: Move, evalMove :: Evaluation, evalBest :: Evaluation, msComparison :: Int} deriving Show

ms :: Color -> Move -> Move -> Evaluation -> Evaluation -> MoveSummary
ms col mv mvBest eval evalBest = MoveSummary mv mvBest eval evalBest compFull
  where comp = (evaluationNumber eval) - (evaluationNumber evalBest) 
        comparison = if col == White then (-comp) else comp
        compFull = if playedBest then 0 else min 0 comparison
        playedBest = mv == mvBest

formatBest :: [(Move, StockfishMove, GameState)] -> [(MoveSummary, GameState)]
formatBest (first : second : rest) = (mvs, gs) : (formatBest (second:rest))
  where mvs = ms col mv mvBest (sfEvaluation sf) (sfEvaluation sfAfter)
        col = gs ^. gsColor
        mvBest = sfMove sf
        (mv, sf, gs) = first
        (_, sfAfter, _) = second
formatBest _ = []

-- data BestMoveTest = BestMoveTest { moveTestFen :: String, moveTestMove :: String, moveTestBest :: String, moveTestComparison :: Int} deriving (Show, Generic)

-- instance ToJSON BestMoveTest

-- printBest :: (MoveSummary, GameState) -> BestMoveTest
-- printBest (mv, gs) = BestMoveTest gsFen mvString mvBestString comp
--   where gsFen = gameStateToFen gs
--         mvString = shortMove (msMove mv)
--         mvBestString = shortMove (msMoveBest mv)
--         comp = msComparison mv
        
-- writeFile "/home/cg/data/output/tests.json" $ U.toString $ encode $ filter (\mt -> (moveTestComparison mt) < -150) $ concat $ (fmap . fmap) printBest $ fmap formatBest b
        

