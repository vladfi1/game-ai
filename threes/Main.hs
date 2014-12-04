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
import TrainNN

import AI.Training
import AI.Calculation


humanPlayer game @ PlayerState {actions} = do
  print game
  line <- getLine
  let tile = read line :: Direction
  return $ (Map.fromList actions) Map.! tile

cpuPlayer = (lookAheadPlayDepth 4 basicHeuristic)

saveDir = "saved/threes/"

testRunner = do
  recordGame saveDir newGame cpuPlayer
  
  games <- readDir saveDir
  
  let (game :: Saved ThreesState) = (games !! 0)
  
  --forM game print
  print game

modelConfig = ModelConfig
  { activation = Sigmoid
  , costModel = MeanSquared
  , layers = [1]
  , regularization = 0.001
  }

train dir = do
  dataset <- loadData dir scoreThrees
  
  model <- initNN modelConfig
  
  return ()
  

main = do
  setStdGen $ mkStdGen 0
  testRunner

