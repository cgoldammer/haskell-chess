-- | Functionality to read external PGNs. This module deals with getting the
-- data into an intermediate format that can then be used by the `Chess.Pgn.Logic` module
-- to be parsed into a full `Game`.

module Chess.Pgn.External where

import Control.Monad
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.Text as Te
import Control.Lens hiding ((.=))
import Control.Applicative
import qualified Turtle as Tu

-- Importing Pgns starts with an external file.

type PgnMove = String

resultReadValue :: String -> PossibleResult
resultReadValue "1-0" = WhiteWin
resultReadValue "0-1" = BlackWin
resultReadValue "1/2-1/2" = Draw
resultReadValue _ = Draw

resultParser = do
  res <- string "1-0" <|> string "1/2-1/2" <|> string "0-1"
  return $ resultReadValue $ Te.unpack res

namePartParser = many' (digit <|> char '_' <|> letter <|> space <|> char '\'')

firstNameParser :: Parser String
firstNameParser = do
  string ", "
  namePartParser

nameParser :: Parser Player
nameParser = do
  last <- namePartParser
  first :: Maybe String <- option Nothing (Just <$> firstNameParser)
  return $ Player (maybe "" id first) last

nameParse = letter <|> char ',' <|> space

tagParse :: String -> Parser a -> Parser a
tagParse tagName p = do
  string $ Te.pack $ "[" ++ tagName ++ " \""
  content <- p
  string "\"]"
  endOfLine
  return content

fullTagParse :: Parser PgnTag
fullTagParse =  eventParse 
            <|> siteParse
            <|> dateParse
            <|> roundParse
            <|> whitePlayerParse
            <|> blackPlayerParse
            <|> whiteEloParse
            <|> blackEloParse
            <|> resultParse
            <|> otherParse

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

data PossibleResult = WhiteWin | Draw | BlackWin deriving (Eq)

instance Show Player where
  show (Player first last) = first ++ " " ++ last

instance Show PossibleResult where
  show WhiteWin = "1"
  show BlackWin = "0"
  show Draw = "D"


data PgnTag = 
    PgnEvent String
  | PgnSite String
  | PgnOther String String
  | PgnDate String
  | PgnRound Int
  | PgnWhite Player
  | PgnBlack Player
  | PgnWhiteElo Int
  | PgnBlackElo Int
  | PgnResult PossibleResult
  deriving (Show, Eq)

formatForDB :: PgnTag -> (String, String)
formatForDB (PgnEvent s) = ("Event", s)
formatForDB (PgnDate s) = ("Date", s)
formatForDB (PgnRound s) = ("Round", show s)
formatForDB (PgnWhite player) = ("White", show player)
formatForDB (PgnBlack player) = ("Black", show player)
formatForDB (PgnWhiteElo rating) = ("WhiteElo", show rating)
formatForDB (PgnBlackElo rating) = ("BlackElo", show rating)
formatForDB (PgnOther name s) = (name, s)


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



-- | Parses comments for sidelines in a PGN.
-- This is surprisingly non-trivial: In a PGN, there can be arbitrarily many sidelines, each
-- of which can be arbitrarily nested with more moves.
-- Therefore, parsing sidelines is recursive, but a naive recursive implementation simply
-- doesn't complete. Therefore, we are allowing up to 10 sidelines.
sidelineParser :: Parser ()
sidelineParser = do
  many' space
  char '('
  takeTill (\c -> c `elem` ("()"::String))
  many' sidelineParser
  let maxNumberOfParentheses = 10
  replicateM maxNumberOfParentheses insideParser -- This is a hack - we're limiting at finding 10 sidelines
  takeTill (\c -> c `elem` ("()"::String))
  char ')'
  return ()

insideParser = do
  takeTill (\c -> c `elem` ("()"::String))
  many' sidelineParser
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

parseGameMoves :: Parser [PgnMove]
parseGameMoves = do
  many' $ sidelineParser <|> commentParser
  many' space
  moves <- many1' moveParser
  return $ concat moves
  -- return $ concat $ (fmap . fmap) (filter filterMoves) moves

eventParse :: Parser PgnTag = fmap PgnEvent $ tagParse "Event" $ fmap (Te.unpack) $ Data.Attoparsec.Text.takeWhile (/= '\"')
siteParse :: Parser PgnTag = fmap PgnSite $ tagParse "Site" $ many' $ letter <|> space
dateParse :: Parser PgnTag = fmap PgnDate $ tagParse "Date" $ many' $ digit <|> char '.'
roundParse :: Parser PgnTag = fmap (PgnRound . read) $ tagParse "Round" $ many' digit
whitePlayerParse :: Parser PgnTag = fmap PgnWhite $ tagParse "White" $ nameParser
blackPlayerParse :: Parser PgnTag = fmap PgnBlack $ tagParse "Black" $ nameParser
resultParse :: Parser PgnTag = fmap PgnResult $ tagParse "Result" $ resultParser
whiteEloParse :: Parser PgnTag = fmap (PgnWhiteElo . read) $ tagParse "WhiteElo" $ many' digit
blackEloParse :: Parser PgnTag = fmap (PgnBlackElo . read) $ tagParse "BlackElo" $ many' digit

