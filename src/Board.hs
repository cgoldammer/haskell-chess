{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Board (stringToPosition
            , Piece (..)
            , Field (Field, fieldColumn, fieldRow)
            , Position
            , Color (White, Black)
            , PieceField (PieceField, pfField, pfColor, pfPiece)
            , fieldToInt
            , rowInt, colInt
            , intRow
            , invertColor
            , intColumn
            , columnInt
            , allRows
            , stringToPieceField
            , stringToField, stringToMove
            , stringToPiece
            , colorString
            , allColumns
            , shortPiece, shortColumn, shortRow, shortField
            , Column (A, B, C, D, E, F, G, H)
            , Row (R1, R2, R3, R4, R5, R6, R7, R8)
            , Move (Move, moveFrom, moveTo, movePromotionPiece)
            , MoveLocation
            , fieldColor
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

data Field = Field { fieldColumn :: Column, fieldRow :: Row } deriving (Eq, Ord)
data Move = Move {moveFrom :: Field, moveTo :: Field, movePromotionPiece :: Maybe Piece} deriving (Eq, Show, Ord)
type MoveLocation = (Field, Field)
data PieceField = PieceField {pfPiece :: Piece, pfColor :: Color, pfField :: Field} deriving (Eq)
type Position = [PieceField]

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
shortColumn c = [['A'..'H'] !! (columnInt c)]

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
columnInt c = fromJust $ elemIndex c allColumns

rowInt :: Row -> Int
rowInt r = (fromJust (elemIndex r allRows)) + 1

colInt :: Column -> Int
colInt r = (fromJust (elemIndex r allColumns)) + 1

intRow :: Int -> Maybe Row
intRow i = index (i - 1) allRows

intColumn :: Int -> Maybe Column
intColumn = (flip index) allColumns


