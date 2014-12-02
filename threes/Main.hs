{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Functor
import Control.Monad
import Control.Monad.Random
import qualified Data.Map as Map

import Threes
import NewGame
import Runner


humanPlayer game @ Player {actions} = do
  print game
  line <- getLine
  let tile = read line :: Direction
  return $ (Map.fromList actions) Map.! tile

cpuPlayer = return . (lookAheadPlayDepth 4 basicHeuristic)

saveDir = "saved/threes/"

main = do
  setStdGen $ mkStdGen 0
  
  recordGame saveDir newGame cpuPlayer
  
  games <- readGames saveDir
  
  let game = (games !! 0) :: [ThreesState]
  
  forM game print
