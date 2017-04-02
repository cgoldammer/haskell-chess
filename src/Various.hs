{-# LANGUAGE FlexibleInstances, OverlappingInstances, ScopedTypeVariables #-}

module Various (testGS, testGS2, mate1GS, mate1PS, mat) where

import Logic
import Board
import Data.Maybe



testPS = fromJust $ stringToPosition ["WKA1", "BRA8", "BRB8", "BKC8"]
testGS = GameState testPS White
testGS2 = GameState testPS Black

mate1PS = fromJust $ stringToPosition ["WKA1", "WRA7", "WRB6", "BKH8"]
mate1GS = GameState mate1PS White


gsBlackFromString s = GameState (fromJust (stringToPosition s)) Black
mat = gsBlackFromString ["WKA1", "BKH8", "WRB6", "WRC7"]
