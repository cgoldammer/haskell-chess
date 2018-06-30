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

import Chess.Types
import Chess.Helpers

import Data.Maybe (fromJust, Maybe(..), isJust)
import Data.List (elemIndex)
import Control.Monad (join, liftM2)
import Text.Read (readMaybe)

allPieces = [King ..]
allFullPieces = [King .. Knight]
allNonKingPieces = [Queen ..]
allNonKingFullPieces = [Queen .. Knight]
allNonPawnPieces = [King .. Knight]

-- | A `MoveLocation` is a simplified version of a `Move` that denotes only the
-- starting and destination fields.
type MoveLocation = (Field, Field)

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

allColumnNames :: String
allColumnNames = fmap (head . showColumn) allColumns

allRowNames :: String
allRowNames = fmap (head . showRow) allRows

charColumn :: Char -> Maybe Column
charColumn c = join $ fmap (`safeIndex`allColumns) (elemIndex c ['A'..'H'])

charRow :: Char -> Maybe Row
charRow r = join $ fmap (`safeIndex`allRows) (elemIndex r ['1'..'8'])

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
            allPresent = isJust col && isJust piece && isJust c && isJust r

stringToPosition :: [String] -> Maybe Position
stringToPosition = traverse stringToPieceField

invertColor White = Black
invertColor Black = White

flipRow :: Row -> Row
flipRow row = fromJust $ intRow $ 9 - rowInt row

flipField :: Field -> Field
flipField (Field column row) = Field column (flipRow row)

