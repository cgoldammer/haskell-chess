-- | Functionality to read external PGNs. This module deals with getting the
-- data into an intermediate format that can then be used by the `Chess.Pgn.Logic` module
-- to be parsed into a full `Game`.
--
-- This module contains a fair number of hacks. One thing I've noted is that there
-- doesn't seem to be a formal specification for PGNs, so instead my main goal was
-- that I could parse PGNs as exported by Chessbase, chess.com, and external sites.
module Chess.Pgn.External where

import Control.Monad (replicateM)
import Data.Attoparsec.Text (Parser, takeWhile, string, digit, char, letter, space, endOfLine, skipWhile, takeTill, satisfy, inClass)
import Data.Attoparsec.Combinator (many', option, many1')
import Data.Text as Te (Text, pack, unpack)
import Control.Applicative ((<|>))
import Data.Foldable (fold)

type PgnMove = String

data Result = WhiteWin | Draw | BlackWin deriving (Eq)

resultReadValue :: String -> Result
resultReadValue "1-0" = WhiteWin
resultReadValue "0-1" = BlackWin
resultReadValue "1/2-1/2" = Draw
resultReadValue _ = Draw

resultParser = do
  res <- string "1-0" <|> string "1/2-1/2" <|> string "0-1"
  return $ resultReadValue $ unpack res

namePartParser = many' (digit <|> char '_' <|> letter <|> space <|> char '\'')

firstNameParser :: Parser String
firstNameParser = do
  string ", "
  namePartParser


-- In PGNs, names for over-the-board games are provided in the form of lastname, firstname.
-- However, in online games, it's usual to just report the username. So we parse the name
-- into a mandatory last name (everything before a comma) and an optional first name
-- after the comma. Note that this will break in the case where a username contains
-- a comma, or various other non-standard characters.
nameParser :: Parser Player
nameParser = do
  last <- namePartParser
  first :: Maybe String <- option Nothing (Just <$> firstNameParser)
  return $ Player (maybe "" id first) last

nameParse = letter <|> char ',' <|> space

-- The default logic for parsing tags. All specific parsers (e.g. a name parser)
-- will work with the inside content that's parsed from here. 
tagParse :: String -> Parser a -> Parser a
tagParse tagName p = do
  string $ pack $ "[" ++ tagName ++ " \""
  content <- p
  string "\"]"
  endOfLine
  return content

eventParse :: Parser PgnTag = fmap PgnEvent $ tagParse "Event" $ fmap unpack $ Data.Attoparsec.Text.takeWhile (/= '\"')
siteParse :: Parser PgnTag = fmap PgnSite $ tagParse "Site" $ many' $ letter <|> space
dateParse :: Parser PgnTag = fmap PgnDate $ tagParse "Date" $ many' $ digit <|> char '.'
roundParse :: Parser PgnTag = fmap (PgnRound . read) $ tagParse "Round" $ many' digit
whitePlayerParse :: Parser PgnTag = fmap PgnWhite $ tagParse "White" $ nameParser
blackPlayerParse :: Parser PgnTag = fmap PgnBlack $ tagParse "Black" $ nameParser
resultParse :: Parser PgnTag = fmap PgnResult $ tagParse "Result" $ resultParser
whiteEloParse :: Parser PgnTag = fmap (PgnWhiteElo . read) $ tagParse "WhiteElo" $ many1' digit
blackEloParse :: Parser PgnTag = fmap (PgnBlackElo . read) $ tagParse "BlackElo" $ many1' digit


-- | The full set of possible tags. The `otherParse` is a catch-all, and everything
-- that can't be parsed exactly parsed into that structure.
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
  event :: Text <- Data.Attoparsec.Text.takeWhile (\c -> c /='\"')
  string "\"]"
  endOfLine
  return $ PgnOther tagName (unpack event)
  
data Player = Player {firstName :: String, lastName :: String} deriving (Eq)

instance Show Player where
  show (Player first last) = first ++ " " ++ last

instance Show Result where
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
  | PgnResult Result
  deriving (Show, Eq)

-- |Provide a nicely formatted tupe of strings to serialized tags in the
-- database
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
  first <- satisfy $ inClass $ ['a'..'h'] ++ "KNBQRO"
  rest <- many1' $ fold $ [letter, digit] ++ fmap char ("#x+=O-")
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

