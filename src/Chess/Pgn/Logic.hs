-- | Functionality to read a PGN into a full parsed `PgnGame`.
-- A PgnGame consists of both a `Game` and a list of game tags
-- and it represents the full logic of the PGN in a structured format.

module Chess.Pgn.Logic (
    pgnToMove, cleanPgn --, -- pgnAsMoves
  , PgnType (Standard, WithColumn, WithRow, WithBoth)
  , possibleMoveFields
  , pgnPiece
  , pgnToTargetField, pgnToPromotion
  , PgnTag (..), Player (..)
  , PgnGame (..), Game(..)
  , PossibleResult(..)
  , MoveSummary (..)
  , ParsedGame 
  , tagParse
  , eventParse, siteParse, fullTagParse, parseAllTags
  , parseGameMoves, moveParser, bothMoveParser, sidelineParser
  , readSingleGame
  , gameSummaries
  , gamePgnMoves, gamePgnFull
  , createPgnMoves, expandMove
  , gameText
  , unsafeMoves
  , BlunderThreshold
  , GameEvaluation(..)
  , getGames
  , allPgnsForMove, moveAsPgn, allOtherPgns
  , moveToPgn) where

import Debug.Trace
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
import qualified Filesystem.Path.CurrentOS as FS
import qualified Turtle as Tu
import Data.String.Utils as SUtils

import Chess.Board
import Chess.Logic
import Chess.Helpers
import Chess.Stockfish
import Chess.Pgn.External as PgnExternal

pgnToPromotion :: PgnMove -> (PgnMove, Maybe Piece)
pgnToPromotion pgn
  | (end `elem` ("QNBR" :: String)) = (init pgn, stringToPiece [end])
  | otherwise = (pgn, Nothing)
  where end = last pgn
        secondToLast = head $ tail pgn

renameCastles :: PgnMove -> Color -> PgnMove
renameCastles "O-O" White = "Kg1"
renameCastles "O-O" Black = "Kg8"
renameCastles "O-O-O" White = "Kc1"
renameCastles "O-O-O" Black = "Kc8"
renameCastles pgnMove _ = pgnMove

isPromotionMove :: Move -> Bool
isPromotionMove (PromotionMove _ _ _) = True
isPromotionMove _ = False

possibleMoveFields :: GameState -> PgnMove -> [((Piece, Move), [String])]
possibleMoveFields gs pgn = zip movesWithPiece pgnMoves
    where   
            allMoves = filter (\(piece, _) -> piece == movePiece) $ allNextLegalMoves gs
            pgnCleaned = renameCastles (filter filterMoves pgn) color
            (pgnWithoutPromotion, promotionPiece) = pgnToPromotion pgnCleaned
            relevantMoves = filter (if (isJust promotionPiece) then (isPromotionMove . snd) else (not . isPromotionMove. snd)) allMoves
            targetField = pgnToTargetField pgnWithoutPromotion
            movePiece = pgnPiece pgnWithoutPromotion
            color = gs ^. gsColor
            movesWithPieceFields = [(mv, PieceField piece color (mv ^. moveFrom)) | (piece, mv) <- relevantMoves, targetField == Just (mv ^. moveTo)]
            movesWithPiece = [(pf ^. pfPiece, m) | (m, pf) <- movesWithPieceFields]
            error = "PGN" ++ pgnCleaned ++ "Pro:" ++ show (pgnToPromotion pgnCleaned) ++ "Target field" ++ show targetField ++ "PF:" ++ show movesWithPieceFields
            pgnMoves = createPgnMoves gs movesWithPieceFields

pgnToTargetField pgn = stringToField $ fmap Ch.toUpper $ reverse $ Data.List.take 2 $ reverse pgn

pgnToMove :: MoveReader
pgnToMove gs pgn = listToMaybe [mv | (mv, pgnList) <- possible, pgn `elem` pgnList]
    where color = gs ^. gsColor
          possible = possibleMoveFields gs pgn

cleanPgn :: PgnMove -> Color -> PgnMove
cleanPgn pgn color = withoutPromotion
  where (withoutPromotion, _) = pgnToPromotion pgn 

-- pgnParse pgn color

pgnPiece :: PgnMove -> Piece
pgnPiece pgnMove
  | head pgnMove `elem` ("KQRNB" :: String) = fromJust $ stringToPiece $ [head pgnMove]
  | otherwise = Pawn

createPgnMoves :: GameState -> [(Move, PieceField)] -> [[PgnMove]]
createPgnMoves gs ls = fmap (uncurry expander) ls
  where expander mv pf = expandMove gs mv pf

-- replaceString :: Move -> (String, String)
-- replaceString (CastlingMove e1 g1 h1 f1) = "O-O"
-- replaceString (CastlingMove e1 c1 a1 f1) = "O-O-O"
-- replaceString (CastlingMove e8 g8 h8 f8) = "O-O"
-- replaceString (CastlingMove e8 c8 a8 d8) = "O-O-O"
-- replaceString _ = ("_", "_")

-- addCastleFormat :: Move -> PieceField -> String -> String
-- addCastleFormat mv pf pgn = uncurry (\mv pf -> SUtils.replace mv pf pgn) $ replaceString mv

-- expandMoveFormatted :: GameState -> Move -> PieceField -> [PgnMove]
-- expandMoveFormatted gs mv pf = fmap (addCastleFormat mv pf) $ expandMove gs mv pf

expandMove :: GameState -> Move -> PieceField -> [PgnMove]
expandMove _ cm@(CastlingMove _ _ _ _) _ = [show cm]
expandMove gs mv pf = expanded 
    where   withColumn = moveHelper WithColumn mv pf
            withNeither = moveHelper Standard mv pf
            withRow = moveHelper WithRow mv pf
            withBoth = moveHelper WithBoth mv pf
            piece = pf ^. pfPiece
            isCheck = inCheck $ move gs piece mv
            isEp = Just (mv ^. moveTo) == (gs ^. gsEnPassantTarget) && piece == Pawn
            isTake = (isTaking gs (mv ^. moveTo)) || isEp
            moveHelper = moveToPgn isCheck isTake
            expandedFull = [withNeither, withColumn, withRow, withBoth]
            expanded = case () of _
                                    | (piece == Pawn) && isTake -> [withColumn]
                                    | (piece == Pawn) && not isTake -> [withNeither]
                                    | otherwise -> expandedFull

data PgnType = Standard | WithColumn | WithRow | WithBoth

convertPgnTypeToBools :: PgnType -> (Bool, Bool)
convertPgnTypeToBools Standard = (False, False)
convertPgnTypeToBools WithColumn = (True, False)
convertPgnTypeToBools WithRow = (False, True)
convertPgnTypeToBools WithBoth = (True, True)

moveToPgn :: Bool -> Bool -> PgnType -> Move -> PieceField -> PgnMove
moveToPgn isCheck isTake pgnType mv pf = uncurry helper $ bools
  where helper = moveToPgnHelper isCheck isTake mv pf 
        bools = convertPgnTypeToBools pgnType

promotionString :: Move -> Maybe String
promotionString (PromotionMove _ _ piece) = Just $ showPiece piece
promotionString _ = Nothing

moveToPgnHelper :: Bool -> Bool -> Move -> PieceField -> Bool -> Bool -> PgnMove
moveToPgnHelper isCheck isTake mv pf withColumn withRow = concat $ catMaybes values
  where
    pieceString = pgnPieceChar $ pf ^. pfPiece
    columnString = fmap Ch.toLower $ showColumn $ (mv ^. moveFrom) ^. fieldColumn
    rowString = showRow $ mv ^. moveFrom ^. fieldRow
    targetString = fmap Ch.toLower $ showField $ mv ^. moveTo
    takeString = makeMaybe isTake "x"
    checkString = makeMaybe isCheck "+"
    promoteString = fmap ('=':) $ promotionString mv
    values = [Just pieceString, makeMaybe withColumn columnString, makeMaybe withRow rowString, takeString, Just targetString, promoteString, checkString]


pgnPieceChar :: Piece -> String
pgnPieceChar piece = filter (not . (`elem` ("P"::String))) $ showPiece piece

gamePgnMoves :: Game -> [PgnMove]
gamePgnMoves (Game startGs _ mvs) = snd $ foldl nextPgnMove (startGs, []) mvs

gamePgnFull :: Game -> String
gamePgnFull = addNumbers . gamePgnMoves

addNumbers :: [PgnMove] -> String
addNumbers s = concat [ show num ++ "." ++ move | (num, move) <- zipped]
  where paired = splitPaired s
        range = [1..(length paired)]
        zipped = zip range paired
        
splitPaired s = splitIntoPairs s []

splitIntoPairs (a:b:rest) accum = accum ++ ((a ++ " " ++ b ++ " ") : splitIntoPairs rest accum)
splitIntoPairs (a:[]) accum = accum ++ [a]
splitIntoPairs [] accum = accum

nextPgnMove :: (GameState, [PgnMove]) -> Move -> (GameState, [PgnMove])
nextPgnMove (gs, pgnMoves) mv = (gs', pgnMoves ++ [pgnMove])
  where gs' = move' gs mv
        pgnMove = moveAsPgn gs mv

-- Find all pgnMoves for this move
-- Find possible moves and piecefields for all other pieces
-- Use the first pgnMove that's not in the other moves
moveAsPgn :: GameState -> Move -> PgnMove
moveAsPgn gs mv = head $ [pgnM | pgnM <- allPgns, not (pgnM `elem` allOther)]
  where allOther = allOtherPgns gs mv
        allPgns = allPgnsForMove gs mv

allPgnsForMove :: GameState -> Move -> [PgnMove]
allPgnsForMove gs mv = expandMove gs mv pieceField
  where color = gs ^. gsColor
        piece = movePiece gs mv
        from = mv ^. moveFrom
        pieceField = PieceField piece color from
        newState = move gs piece mv

allOtherPgns :: GameState -> Move -> [PgnMove]
allOtherPgns gs mv = concat $ fmap (uncurry (flip (expandMove gs))) allMovesWithPieceFields
  where piece = movePiece gs mv
        color = gs ^. gsColor
        from = mv ^. moveFrom
        pieceField = PieceField piece color from
        allMoves = allNextLegalMoves gs
        allMovesWithPieceFields = [(PieceField p color (m ^. moveFrom), m) | (p, m) <- allMoves, from /= m ^. moveFrom]

-- | A PgnGame consists of both a `Game` and a list of game tags. It represents
-- the full information content of a PGN.
data PgnGame = PgnGame { 
    pgnGameTags :: [PgnTag]
  , parsedPgnGame :: Game } deriving (Show)

unsafeMoves :: Te.Text -> [PgnMove]
unsafeMoves s = fromJust $ EitherC.rightToMaybe $ parseOnly parseGameMoves s


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

parseGame :: [PgnTag] -> [PgnMove] -> ParsedGame
parseGame tags moves = gameParser game
  where game = gameFromStart pgnToMove moves
        gameParser (Left s) = Left $ PgnParseError s
        gameParser (Right g) = Right $ PgnGame tags g
  
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

gameSummaries :: Game -> IO [MoveSummary]
gameSummaries g = do 
  let moves = gameMoves g
  let gameStates = scanl move' (startingGameState g) moves
  bestMoves <- bestStockfishMoves$ zip gameStates moves
  return $ gameEvalSummary bestMoves
  
type BlunderThreshold = Int

-- gameBlunders :: PgnGame -> BlunderThreshold -> IO GameEvaluation
-- gameBlunders g bt = do
--   summaries <- gameSummaries g
--   let blunders = filter (\ms -> msComparison ms <= (- bt)) $ fmap fst summaries
--   let gt = pgnGameTags g
--   return $ GameEvaluation gt blunders

-- gameCSV :: PgnGame -> IO String
-- gameCSV gm = do 
--   let gt = pgnGameTags gm
--   let g = parsedPgnGame gm
--   let moves = gameMoves g
--   let start = startingGameState g
--   let gameStates = scanl move' start moves
--   mv <- bestStockfishMoves$ zip gameStates moves
--   return $ gameSummary gt mv

bestStockfishMoves:: [(GameState, Move)] -> IO [(Move, Maybe StockfishMove, GameState)]
bestStockfishMoves positionsRaw = do
  let numberMoves = length positionsRaw
  let (start, end) = interestingRange numberMoves
  let (start, end) = (0, numberMoves)
  let positions = slice start end positionsRaw
  let mvs = fmap snd positions
  let states = fmap fst positions
  bests <- mapM (\gs -> singleBestMove (gameStateToFen gs) 100 1) states
  return $ zip3 mvs bests states

gameEvalSummary :: [(Move, Maybe StockfishMove, GameState)] -> [MoveSummary]
gameEvalSummary summ = fmap fst $ formatBest $ keepOnlyJust summ

keepOnlyJust summ = fmap (\(m, sfm, gs) -> (m, fromJust sfm, gs)) $ Data.List.takeWhile (\(_, sfm, gs) -> isJust sfm) summ

-- gameSummary :: [PgnTag] -> [(Move, Maybe StockfishMove, GameState)] -> String
-- gameSummary gt summ = tagSummaries ++ "," ++ evalSummaries
--   where evals = gameEvaluations summ
--         (whiteEval, blackEval) = evalSummary evals
--         tagSummaries = Data.List.intercalate "," $ catMaybes $ fmap tagSummary gt
--         evalSummaries = show (evalAverage whiteEval) ++ "," ++ show (evalAverage blackEval)

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

-- gameEvaluations :: [(Move, Maybe StockfishMove, GameState)] -> [(Color, Int)]
-- gameEvaluations summ = results
--   where formatted = gameEvalSummary summ
--         results = [(gs ^. gsColor, msComparison ms) | (ms, gs) <- formatted]
        
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
        

