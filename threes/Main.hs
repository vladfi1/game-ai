{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Pipes
import qualified Pipes.Prelude as P

import Data.Functor
import Control.Monad
import Control.Monad.Random
import qualified Data.Map as Map

import Threes
import NewGame


humanPlayer game @ Player {actions} = do
  print game
  line <- getLine
  let tile = read line :: Direction
  return $ (Map.fromList actions) Map.! tile

cpuPlayer = return . (lookAheadPlayDepth 8 basicHeuristic)

makePlayer player game @ Player {} = player game
makePlayer player game @ Nature {} = naturePlayer game

main = runEffect $
  (playOutM (makePlayer cpuPlayer) newGame) >->
  (P.filter isPlayer) >->
  P.print

