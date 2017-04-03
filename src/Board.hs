{-# LANGUAGE FlexibleInstances, OverlappingInstances, ScopedTypeVariables #-}
module Board (stringToPosition
            , Piece (..)
            , Field
            , Position
            , Color (White, Black)
            , PieceField (PieceField, pfField, pfColor, pfPiece)
            , fieldToInt
            , rowInt
            , intRow
            , invertColor
            , intColumn
            , columnInt
            , allRows
            , stringToPieceField
            , stringToField
            , allColumns
            , shortPiece
            , Column (A, B, C, D, E, F, G, H)
            , Row (R1, R2, R3, R4, R5, R6, R7, R8)
            , Move (Move, moveFrom, moveTo, movePromotionPiece)
            , MoveLocation
            , allPieces, allFullPieces, allNonKingPieces, allNonKingFullPieces, allNonPawnPieces
        ) where 

import Helpers
import Data.Maybe
import Data.List
import Control.Monad
import Text.Read

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

type Field = (Column, Row)
data Move = Move {moveFrom :: Field, moveTo :: Field, movePromotionPiece :: Maybe Piece} deriving (Eq, Show, Ord)
type MoveLocation = (Field, Field)
data PieceField = PieceField {pfPiece :: Piece, pfColor :: Color, pfField :: Field} deriving (Eq)
type Position = [PieceField]


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
shortColumn c = [['A'..'H'] !! (columnInt c)]

shortRow :: Row -> String
shortRow r = show $ rowInt r

shortField :: Field -> String
shortField (c, r) = shortColumn c ++ shortRow r

instance Show Color where
    show = shortColor

instance Show Piece where
    show = shortPiece

instance Show Row where
    show = shortRow

instance Show Column where
    show = shortColumn

instance Show Field where
    show (c, r) = show c ++ show r

instance Show PieceField where
    show (PieceField p c f) = show c ++ show p ++ show f

charColumn :: Char -> Maybe Column
charColumn c = join $ fmap (flip index allColumns) (elemIndex c ['A'..'H'])

charRow :: Char -> Maybe Row
charRow r = join $ fmap (flip index allRows) (elemIndex r ['1'..'8'])


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

sequence2Tuple = uncurry $ liftM2 (,)

stringToField :: String -> Maybe Field
stringToField [c, r] = sequence2Tuple (charColumn c, join (fmap intRow (readMaybe [r])))

stringToPieceField :: String -> Maybe PieceField 
stringToPieceField [colS, pieceS, cS, rS]
    | allPresent = Just $ PieceField (fromJust piece) (fromJust col) ((fromJust c), (fromJust r))
    | otherwise = Nothing
    where   col = colorString [colS]
            piece = stringToPiece [pieceS]
            c = charColumn cS
            r = charRow rS
            allPresent = (isJust col) && (isJust piece) && (isJust c) && (isJust r)

stringToPosition :: [String] -> Maybe Position
stringToPosition = traverse stringToPieceField

fieldToInt :: Field -> (Int, Int)
fieldToInt (c, r) = (columnInt c, rowInt r)

invertColor White = Black
invertColor Black = White

columnInt :: Column -> Int
columnInt c = fromJust $ elemIndex c allColumns

rowInt :: Row -> Int
rowInt r = (fromJust (elemIndex r allRows)) + 1

intRow :: Int -> Maybe Row
intRow i = index (i - 1) allRows

intColumn :: Int -> Maybe Column
intColumn = (flip index) allColumns


