{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
module Main (main) where

import Algorithms
import Board
import Logic
import Pgn
import Data.Text as T
import Data.Attoparsec.Text hiding (take, D, takeWhile)
import Data.Maybe
import Debug.Trace
import qualified Turtle as Tu
import qualified Data.Text as Te
import Data.Either.Combinators as EitherC
import Data.List

main = do
  gamePgnRaw :: Te.Text <- Tu.strict $ Tu.input "test/files/many.pgn"
  let number = 100
  let needle = "[Event "
  let elements = Data.List.take (number + 1) $ Te.splitOn needle gamePgnRaw
  let gamePgn = Te.intercalate needle elements
  let eitherGame = parseOnly parseWholeGames gamePgn
  let games = catMaybes $ fromJust $ EitherC.rightToMaybe eitherGame
  print "NUMBER OF GAMES PARSED"
  print $ Data.List.length games

-- 7/30, 11pm: 17.4s for n=40 games
-- Speed from 19.7 to 5.5s using checking routes
-- 4.3s using king checks controlling fields
-- 3.0s
--
-- Latest: 100 games in 6.2s
-- 7.1s
