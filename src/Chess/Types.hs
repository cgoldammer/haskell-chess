module Chess.Types where

import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Chess.Helpers
import Control.Lens (makeLenses)

type CastlingRights = PlayerData (CastlingData Bool)

data PlayerData a = PlayerData {
    dataWhite :: a
  , dataBlack :: a
} deriving (Eq, Show)

data CastlingData a = CastlingData {
    canKingSide :: a
  , canQueenSide :: a
} deriving (Eq, Show)

instance Functor CastlingData where
  fmap f (CastlingData king queen) = CastlingData (f king) (f queen)

type CastleKingSide = Bool


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

rowInt :: Row -> Int
rowInt r = (fromJust (elemIndex r allRows)) + 1

intRow :: Int -> Maybe Row
intRow i = safeIndex (i - 1) allRows

intColumn :: Int -> Maybe Column
intColumn i = safeIndex (i - 1) allColumns

fieldToInt :: Field -> (Int, Int)
fieldToInt (Field c r) = (columnInt c, rowInt r)

columnInt :: Column -> Int
columnInt c = (fromJust (elemIndex c allColumns)) + 1

-- The chess pieces, ordered by value (starting with the King)
data Piece = King | Queen | Rook | Bishop | Knight | Pawn deriving (Enum, Eq, Ord)


-- | A `PieceField` describes a specific piece that is positioned on a specific field on the board.
-- For instance, this could be a "Black bishop on a7", which includes the piece, the color
-- and the field.
data PieceField = PieceField {_pfPiece :: Piece, _pfColor :: Color, _pfField :: Field} deriving (Eq, Ord)
makeLenses ''PieceField


-- A chess position is simply a list of piece fields, and if we combine a chess position with other
-- metadata about the game history, for instance castling rights, we get the full game state.
type Position = [PieceField]

-- |A `GameState` describes the current game position fully. There is a one-to-one relationship
-- between a `GameState` and a fen position 
-- <https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation>.
data GameState = GameState {
      _gsPosition :: Position
    , _gsColor :: Color
    , _gsCastlingRights :: CastlingRights
    , _gsEnPassantTarget :: Maybe Field
    , _gsHalfMove :: Int
    , _gsFullMove :: Int} deriving (Eq, Show)

makeLenses ''GameState

data Move = StandardMove { _moveFrom :: Field, _moveTo :: Field }
    | PromotionMove { _moveFrom :: Field, _moveTo :: Field, _promotionPiece :: Piece }
    | EnPassantMove { _moveFrom :: Field, _moveTo :: Field, pawnCaptured :: Field }
    | CastlingMove { _moveFrom :: Field, _moveTo :: Field, _rookFrom :: Field, _rookTo :: Field }
    deriving (Eq, Ord)

makeLenses ''Move
