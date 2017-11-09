{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
module Main (main) where

import Chess.Algorithms
import Chess.Board
import Chess.Logic
import Chess.Pgn

import Data.Text as T
import Data.Attoparsec.Text hiding (take, D, takeWhile)
import Data.Maybe
import Data.Either
import Debug.Trace
import qualified Turtle as Tu
import qualified Data.Text as Te
import Data.Either.Combinators as EitherC
import Data.List

main = do
  let number = 100
  let file = "test/files/many.pgn"
  games <- getGames file number 
  print "NUMBER OF GAMES"
  print $ Data.List.length $ games
  print "Correct"
  let (lefts, rights) = Data.List.span Data.Either.isLeft games
  -- let total = length games
  print $ "Total pgns:" ++ show (Data.List.length games)
  print $ "Correctly parsed:" ++ show (Data.List.length rights)

-- stack exec profile-exe +RTS -sstderr
-- 7/30, 11pm: 17.4s for n=40 games
-- Speed from 19.7 to 5.5s using checking routes
-- 4.3s using king checks controlling fields
-- 3.0s
--
-- Latest: 100 games in 6.2s
-- 7.1s
