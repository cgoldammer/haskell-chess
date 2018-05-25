module Chess.Fen (parseFen, fenToGameState, gameStateToFen, castlingRightsParser, fenStringToPosition, fullFen) where

import Data.Attoparsec.Text (Parser, takeWhile, string, digit, char, letter, space, endOfLine, skipWhile, takeTill, satisfy, inClass)
import Data.Attoparsec.Combinator (many1', endOfInput)
import Data.Either.Combinators (rightToMaybe)
import Data.Char (toUpper, toLower)
import Data.String.Utils (replace)
import Data.List (partition, intercalate)
import Data.Maybe (fromJust, Maybe(..), catMaybes, isJust)
import Data.Foldable (fold, foldMap)
import Control.Applicative (liftA2, (<|>), optional)
import Data.Attoparsec.Text (Parser, parseOnly, string, letter, digit, char, space, decimal)
import Control.Lens (makeLenses, view, (^.), over)
import Data.Text (pack)

import Chess.Types
import Chess.Board
import Chess.Helpers

type Fen = String

pieceFieldFen :: Maybe PieceField -> Char
pieceFieldFen Nothing = '1'
pieceFieldFen (Just (PieceField piece color field)) = pieceChar
    where   pieceChar = transformer pieceLetter
            pieceLetter = (showPiece piece) !! 0
            transformer 
                | color == White = id
                | color == Black = toLower

basicFenFromRow :: [PieceField] -> String
basicFenFromRow ps = fmap (\c -> pieceFieldFen (firstPieceOnColumn ps c)) allColumns

aggregateFen :: Int -> String -> String
aggregateFen i = replace (take i (repeat '1')) (show i)

basicFen :: Position -> Fen
basicFen ps = intercalate "/" $ fmap (aggregator . basicFenFromRow) $ positionByRow ps

aggregator s = foldl (flip ($)) s (fmap aggregateFen (reverse [1..8]))

gameStateToFen :: GameState -> Fen
gameStateToFen gs@(GameState ps color cr ept hm fm) = intercalate " " elements
  where elements = ["fen", positionFen, col, castleString, epString, show hm, show fm]
        positionFen = basicFen ps
        col = fmap toLower $ colorToString color 
        castleString = castlingRightsToString cr
        epString = epToString ept


fullFen :: Position -> Fen
fullFen ps = "fen " ++ basicFen ps ++ " w - - 0 1"

fenStringToPosition :: String -> Position
fenStringToPosition s = catMaybes $ fmap stringToPieceField [p ++ f | (p, f) <- collected]
    where
         expanded = concat $ fmap asRepeated $ cleanFenString s
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
fenPieceFormatter x = (fenColorString x):[toUpper x]

-- The main logic for parsing a FEN string into a gamestate
parseFen :: Parser GameState
parseFen = do
  string "fen "
  positionFen :: String <- many1' $ fold [letter, digit, char '/']
  let position = fenStringToPosition positionFen
  space
  playerToMoveString :: Char <- (char 'w' <|> char 'b')
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
castlingRightsToString (PlayerData (CastlingData wk wq) (CastlingData bk bq)) = if length concatenated > 0 then concatenated else "-"
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
firstPieceOnColumn ps c = safeIndex 0 (filter (\pf -> pf ^. pfField . fieldColumn == c) ps)

positionByRow :: Position -> [Position]
positionByRow ps = fmap (piecesOnRow ps) (reverse allRows)

epToString :: Maybe Field -> String
epToString (Just f) = fmap toLower $ showField f
epToString Nothing = "-"

asRepeated :: Char -> String
asRepeated x 
  | x `elem` ['1'..'8'] = take (read [x] :: Int) (repeat '0')
  | otherwise = [x]

fenToGameState :: String -> Maybe GameState
fenToGameState = rightToMaybe . parseOnly parseFen . pack

piecesOnRow :: Position -> Row -> Position
piecesOnRow ps r = filter (\pf -> pf ^. pfField . fieldRow == r) ps

