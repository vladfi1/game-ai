{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Functor
import Control.Monad
import Control.Monad.Random
import qualified Data.Map as Map

import Threes
import NewGame
import Runner


humanPlayer game @ PlayerState {actions} = do
  print game
  line <- getLine
  let tile = read line :: Direction
  return $ (Map.fromList actions) Map.! tile

cpuPlayer = (lookAheadPlayDepth 4 basicHeuristic)

saveDir = "saved/threes/"

testRunner = do
  recordGame saveDir newGame cpuPlayer
  
  games <- readGames saveDir
  
  let (game :: Saved ThreesState) = (games !! 0)
  
  --forM game print
  print game

main = do
  setStdGen $ mkStdGen 0
  testRunner

