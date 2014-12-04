{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Functor
import Control.Monad
import Control.Monad.Random
import qualified Data.Map as Map

import Discrete (choose)
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
  { activation = HyperbolicTangent
  , costModel = MeanSquared
  , layers = [64, 16, numTileClasses]
  , regularization = 0.1
  }

trainConfig = TrainConfig
  { algorithm = LBFGS
  , precision = 0.0001
  , iterations = 100
  }

train dir = do
  dataset <- loadData dir scoreThreesUnary symmetrize
  subsampled <- choose 100 dataset
  model <- initNN modelConfig
  print $ scoreNN model subsampled
  let (trained, score) = trainNN trainConfig subsampled model
  print score

main = do
  setStdGen $ mkStdGen 0
  --testRunner
  train saveDir

