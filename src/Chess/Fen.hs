module Chess.Fen (parseFen, fenToGameState, gameStateToFen, castlingRightsParser, fenStringToPosition, fullFen) where

import Data.Attoparsec.Text (Parser, takeWhile, string, digit, char, letter, space, endOfLine, skipWhile, takeTill, satisfy, inClass, decimal, parseOnly)
import Data.Attoparsec.Combinator (many1', endOfInput)
import Data.Either.Combinators (rightToMaybe)
import Data.Char (toUpper, toLower)
import Data.String.Utils (replace)
import Data.List (partition, intercalate)
import Data.Maybe (fromJust, Maybe(..), catMaybes, isJust)
import Data.Foldable (fold, foldMap)
import Control.Applicative (liftA2, (<|>), optional)
import Control.Lens (makeLenses, view, (^.), over)
import Data.Text (pack)

import Chess.Types
import Chess.Board
import Chess.Helpers

type Fen = String

colorTransformer :: Color -> Char -> Char
colorTransformer White = id
colorTransformer Black = toLower

pieceFieldFen :: Maybe PieceField -> Char
pieceFieldFen Nothing = '1'
pieceFieldFen (Just (PieceField piece color field)) = colorTransformer color pieceLetter 
    where   pieceLetter = head $ showPiece piece

basicFenFromRow :: [PieceField] -> String
basicFenFromRow ps = fmap (pieceFieldFen . firstPieceOnColumn ps) allColumns

aggregateFen :: Int -> String -> String
aggregateFen i = replace (replicate i '1') (show i)

basicFen :: Position -> Fen
basicFen ps = intercalate "/" $ (aggregator . basicFenFromRow) <$> positionByRow ps

aggregator s = foldl (flip ($)) s (fmap aggregateFen (reverse [1..8]))

gameStateToFen :: GameState -> Fen
gameStateToFen gs@(GameState ps color cr ept hm fm) = unwords elements
  where elements = ["fen", positionFen, col, castleString, epString, show hm, show fm]
        positionFen = basicFen ps
        col = toLower <$> colorToString color 
        castleString = castlingRightsToString cr
        epString = epToString ept


fullFen :: Position -> Fen
fullFen ps = "fen " ++ basicFen ps ++ " w - - 0 1"

fenStringToPosition :: String -> Position
fenStringToPosition s = catMaybes $ fmap stringToPieceField [p ++ f | (p, f) <- collected]
    where
         expanded = concatMap asRepeated $ cleanFenString s
         pieceStrings = fmap fenPieceFormatter expanded
         collected = zip pieceStrings allFieldsForFen

cleanFenString :: String -> String 
cleanFenString = filter (not . (`elem` ("/"::String)))

allFieldsForFen :: [String]
allFieldsForFen = fmap show [Field c r | r <- reverse allRows, c <- allColumns]

fenColorString :: Char -> Char
fenColorString x
  | x `elem` ['a'..'z'] = 'B'
  | x `elem` ['A'..'Z'] = 'W'
  | otherwise = ' '

fenPieceFormatter :: Char -> String
fenPieceFormatter x = fenColorString x:[toUpper x]

-- The main logic for parsing a FEN string into a gamestate
parseFen :: Parser GameState
parseFen = do
  string "fen "
  positionFen :: String <- many1' $ fold [letter, digit, char '/']
  let position = fenStringToPosition positionFen
  space
  playerToMoveString :: Char <- char 'w' <|> char 'b'
  let playerToMove = fromJust $ colorString [toUpper playerToMoveString]
  space
  castlingRightsString :: String <- many1' $ foldMap char ("KQkq-" :: String)
  let castlingRights = castlingRightsParser castlingRightsString
  optional space
  epTargetString :: String <- many1' $ fold [letter, digit, char '-']
  let epTarget = stringToField $ fmap toUpper epTargetString
  space
  halfMove :: Int <- decimal
  space
  fullMove :: Int <- decimal
  endOfInput
  return $ GameState position playerToMove castlingRights epTarget halfMove fullMove

castlingRightsParser :: String -> CastlingRights
castlingRightsParser s = PlayerData (CastlingData wK wQ) (CastlingData bK bQ)
  where [wK, bK, wQ, bQ] = fmap (`elem` s) "KkQk"

castlingRightsToString :: CastlingRights -> String
castlingRightsToString (PlayerData (CastlingData wk wq) (CastlingData bk bq)) = if not (null concatenated) then concatenated else "-"
  where wkC = castlingRightsChar wk White True
        wqC = castlingRightsChar wq White False
        bkC = castlingRightsChar bk Black True
        bqC = castlingRightsChar bq Black False
        values = [wkC, wqC, bkC, bqC]
        concatenated = concat values

castlingRightsChar :: Bool -> Color -> Bool -> String
castlingRightsChar True White True = "K"
castlingRightsChar True White False = "Q"
castlingRightsChar True Black True = "k"
castlingRightsChar True Black False = "q"
castlingRightsChar False _ _ = ""

firstPieceOnColumn :: Position -> Column -> Maybe PieceField
firstPieceOnColumn ps c = safeIndex 0 $ filter ((==c) . _fieldColumn . _pfField) ps

positionByRow :: Position -> [Position]
positionByRow ps = fmap (piecesOnRow ps) (reverse allRows)

epToString :: Maybe Field -> String
epToString (Just f) = toLower <$> showField f
epToString Nothing = "-"

asRepeated :: Char -> String
asRepeated x 
  | x `elem` ['1'..'8'] = replicate (read [x] :: Int) '0'
  | otherwise = [x]

fenToGameState :: String -> Maybe GameState
fenToGameState = rightToMaybe . parseOnly parseFen . pack

piecesOnRow :: Position -> Row -> Position
piecesOnRow ps r = filter ((==r) . _fieldRow . _pfField)  ps

