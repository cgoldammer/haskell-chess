module Chess.Board (stringToPosition
            , Piece (..)
            , Field (Field), fieldColumn, fieldRow
            , Position
            , Color (White, Black)
            , PieceField (PieceField)
            , Move (Move), moveFrom, moveTo, movePromotionPiece
            , pfField, pfColor, pfPiece
            , fieldToInt
            , rowInt, columnInt
            , intRow
            , invertColor
            , intColumn
            , allRows
            , stringToPieceField
            , stringToField, stringToMove
            , stringToPiece
            , colorString, colorToString
            , allColumns
            , shortPiece, shortColumn, shortRow, shortField, shortMove
            , Column (A, B, C, D, E, F, G, H)
            , Row (R1, R2, R3, R4, R5, R6, R7, R8)
            , MoveLocation
            , fieldColor
            , allPieces, allFullPieces, allNonKingPieces, allNonKingFullPieces, allNonPawnPieces
        ) where 

import Chess.Helpers

import Data.Maybe
import Data.List
import Control.Monad
import Text.Read
import Control.Lens

data Piece = King | Queen | Rook | Bishop | Knight | Pawn deriving (Enum, Eq, Ord)
data Column = A | B | C | D | E | F | G | H deriving (Enum, Ord, Eq)
data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Enum, Ord, Eq)

allColumns = [A ..]
allRows = [R1 ..]

allPieces = [King ..]
allFullPieces = [King .. Knight]
allNonKingPieces = [Queen ..]
allNonKingFullPieces = [Queen .. Knight]
allNonPawnPieces = [King .. Knight]

data Color = White | Black deriving (Enum, Ord, Eq)

data Field = Field { _fieldColumn :: Column, _fieldRow :: Row } deriving (Eq, Ord)
makeLenses ''Field


data Move = Move {_moveFrom :: Field, _moveTo :: Field, _movePromotionPiece :: Maybe Piece} deriving (Eq, Ord)
makeLenses ''Move

type MoveLocation = (Field, Field)

data PieceField = PieceField {_pfPiece :: Piece, _pfColor :: Color, _pfField :: Field} deriving (Eq)
makeLenses ''PieceField

type Position = [PieceField]

instance Show Move where
  show = shortMove

shortMove :: Move -> String
shortMove (Move from to pr) = fromS ++ toS ++ p
  where fromS = shortField from
        toS = shortField to
        p = prShow pr
    
prShow :: Maybe Piece -> String
prShow Nothing = ""
prShow (Just p) = shortPiece p

fieldColor :: Field -> Color
fieldColor (Field c r)
    | isEven (rowInt r + columnInt c) = Black -- E.g. (A, 1) has positions 1+1=2 and is black
    | otherwise = White
    where isEven i = mod i 2 == 0

shortColor :: Color -> String
shortColor White = "W"
shortColor Black = "B"

shortPiece :: Piece -> String
shortPiece Knight = "N"
shortPiece King = "K"
shortPiece Queen = "Q"
shortPiece Rook = "R"
shortPiece Bishop = "B"
shortPiece Pawn = "P"

shortColumn :: Column -> String
shortColumn c = [['A'..'H'] !! ((columnInt c) - 1)]

shortRow :: Row -> String
shortRow r = show $ rowInt r

shortField :: Field -> String
shortField (Field c r) = shortColumn c ++ shortRow r

instance Show Color where
    show = shortColor

instance Show Piece where
    show = shortPiece

instance Show Row where
    show = shortRow

instance Show Column where
    show = shortColumn

instance Show Field where
    show (Field c r) = show c ++ show r

instance Show PieceField where
    show (PieceField p c f) = show c ++ show p ++ show f

charColumn :: Char -> Maybe Column
charColumn c = join $ fmap (flip safeIndex allColumns) (elemIndex c ['A'..'H'])

charRow :: Char -> Maybe Row
charRow r = join $ fmap (flip safeIndex allRows) (elemIndex r ['1'..'8'])

stringToPiece :: String -> Maybe Piece
stringToPiece "K" = Just King
stringToPiece "R" = Just Rook
stringToPiece "B" = Just Bishop
stringToPiece "Q" = Just Queen
stringToPiece "N" = Just Knight
stringToPiece "P" = Just Pawn
stringToPiece _ = Nothing

colorString :: String -> Maybe Color
colorString "W" = Just White
colorString "B" = Just Black
colorString _ = Nothing

colorToString :: Color -> String
colorToString White = "W"
colorToString Black = "B"

stringToField :: String -> Maybe Field
stringToField [c, r] = liftM2 Field (charColumn c) (join (fmap intRow (readMaybe [r])))
stringToField _ = Nothing

stringToMove :: String -> Maybe Move
stringToMove (c1 : c2 : c3 : c4 : rest) = join $ makeMaybe parseSucceeded $ liftM2 (\f t -> Move f t promotionPiece) from to
  where from = stringToField [c1, c2]
        to = stringToField [c3, c4]
        promotionPieceAttempt = case rest of 
          [] -> Left Nothing
          otherwise -> Right $ stringToPiece rest
        (promotionPiece, parseSucceeded) = case promotionPieceAttempt of
          Left Nothing -> (Nothing, True)
          Right Nothing -> (Nothing, False)
          Right val -> (val, True)


stringToPieceField :: String -> Maybe PieceField 
stringToPieceField [colS, pieceS, cS, rS]
    | allPresent = Just $ PieceField (fromJust piece) (fromJust col) (Field (fromJust c) (fromJust r))
    | otherwise = Nothing
    where   col = colorString [colS]
            piece = stringToPiece [pieceS]
            c = charColumn cS
            r = charRow rS
            allPresent = (isJust col) && (isJust piece) && (isJust c) && (isJust r)

stringToPosition :: [String] -> Maybe Position
stringToPosition = traverse stringToPieceField

fieldToInt :: Field -> (Int, Int)
fieldToInt (Field c r) = (columnInt c, rowInt r)

invertColor White = Black
invertColor Black = White

columnInt :: Column -> Int
columnInt c = (fromJust (elemIndex c allColumns)) + 1

rowInt :: Row -> Int
rowInt r = (fromJust (elemIndex r allRows)) + 1

intRow :: Int -> Maybe Row
intRow i = safeIndex (i - 1) allRows

intColumn :: Int -> Maybe Column
intColumn i = safeIndex (i - 1) allColumns


