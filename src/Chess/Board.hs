-- | This module describes the core physical layout of a chess
-- board, including pieces and moves.

module Chess.Board (stringToPosition
            , Piece (..)
            , Field (Field), fieldColumn, fieldRow
            , Position
            , Color (White, Black)
            , PieceField (PieceField)
            , Move (..)
            , moveFrom, moveTo, promotionPiece, rookFrom, rookTo
            , pfField, pfColor, pfPiece
            , fieldToInt
            , rowInt, columnInt
            , intRow
            , invertColor
            , intColumn
            , allRows
            , stringToPieceField , stringToField , stringToPiece
            , colorString, colorToString
            , allColumns
            , showPiece, showColumn, showRow, showField, showMove
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

-- The chess pieces, ordered by value (starting with the King)
data Piece = King | Queen | Rook | Bishop | Knight | Pawn deriving (Enum, Eq, Ord)

allPieces = [King ..]
allFullPieces = [King .. Knight]
allNonKingPieces = [Queen ..]
allNonKingFullPieces = [Queen .. Knight]
allNonPawnPieces = [King .. Knight]

-- The columns and rows of a chess board
data Column = A | B | C | D | E | F | G | H deriving (Enum, Ord, Eq)
allColumns = [A ..]

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Enum, Ord, Eq)
allRows = [R1 ..]

-- The colors for the two players in a chess game
data Color = White | Black deriving (Enum, Ord, Eq)

-- A field consists of a column and a row.
data Field = Field { _fieldColumn :: Column, _fieldRow :: Row } deriving (Eq, Ord)
makeLenses ''Field

data Move = StandardMove { _moveFrom :: Field, _moveTo :: Field }
    | PromotionMove { _moveFrom :: Field, _moveTo :: Field, _promotionPiece :: Piece }
    | EnPassantMove { _moveFrom :: Field, _moveTo :: Field, pawnCaptured :: Field }
    | CastlingMove { _moveFrom :: Field, _moveTo :: Field, _rookFrom :: Field, _rookTo :: Field }
    deriving (Eq, Ord)
makeLenses ''Move

-- | A `MoveLocation` is a simplified version of a `Move` that denotes only the
-- starting and destination fields.
type MoveLocation = (Field, Field)

-- | A `PieceField` describes a specific piece that is positioned on a specific field on the board.
-- For instance, this could be a "Black bishop on a7", which includes the piece, the color
-- and the field.
data PieceField = PieceField {_pfPiece :: Piece, _pfColor :: Color, _pfField :: Field} deriving (Eq)
makeLenses ''PieceField

-- A chess position is simply a list of piece fields, and if we combine a chess position with other
-- metadata about the game history, for instance castling rights, we get the full game state.
type Position = [PieceField]

instance Show Move where
  show = showMove

showMove :: Move -> String
showMove (StandardMove from to) = showField from ++ showField to
showMove (EnPassantMove from to _) = showMove (StandardMove from to) ++ "EP"
showMove (PromotionMove from to piece) = showMove (StandardMove from to) ++ "=" ++ show piece
showMove (CastlingMove from to rookFrom rookTo) = castlingName to

castlingName :: Field -> String
castlingName (Field G _) = "O-O"
castlingName (Field C _) = "O-O-O"
castlingName _ = undefined
    
prShow :: Maybe Piece -> String
prShow Nothing = ""
prShow (Just p) = showPiece p

fieldColor :: Field -> Color
fieldColor (Field c r)
    | isEven (rowInt r + columnInt c) = Black -- E.g. (A, 1) has positions 1+1=2 and is black
    | otherwise = White
    where isEven i = mod i 2 == 0

showColor :: Color -> String
showColor White = "W"
showColor Black = "B"

showPiece :: Piece -> String
showPiece Knight = "N"
showPiece King = "K"
showPiece Queen = "Q"
showPiece Rook = "R"
showPiece Bishop = "B"
showPiece Pawn = "P"

showColumn :: Column -> String
showColumn c = [['A'..'H'] !! ((columnInt c) - 1)]

allColumnNames :: [Char]
allColumnNames = fmap (head . showColumn) allColumns

allRowNames :: [Char]
allRowNames = fmap (head . showRow) allRows

showRow :: Row -> String
showRow r = show $ rowInt r

showField :: Field -> String
showField (Field c r) = showColumn c ++ showRow r

instance Show Color where
    show = showColor

instance Show Piece where
    show = showPiece

instance Show Row where
    show = showRow

instance Show Column where
    show = showColumn

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

flipRow :: Row -> Row
flipRow row = fromJust $ intRow $ 9 - (rowInt row)

flipField :: Field -> Field
flipField (Field column row) = Field column (flipRow row)

