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

unaryOutput = (scoreThreesUnary, numTileClasses)
binaryOutput = (scoreThreesBits, 4)

(scoreFunction, outputLayer) = unaryOutput


modelConfig = ModelConfig
  { activation = Sigmoid
  , costModel = Logistic
  , layers = [64, 32, 16, outputLayer]
  , regularization = 0.01
  }

trainConfig = TrainConfig
  { algorithm = LBFGS
  , precision = 0.001
  , iterations = 100
  }

train dir = do
  dataset <- loadData dir scoreFunction symmetrize
  
  putStrLn "Subsampling data"
  training <- choose 1000 dataset
  validation <- choose 1000 dataset
  
  putStrLn "Initializing model"
  initial <- initNN modelConfig
  print $ scoreNN initial training
  print $ scoreNN initial validation
  
  putStrLn "Training Model"
  let trained = trainNN trainConfig training initial
  print $ scoreNN trained training
  print $ scoreNN trained validation

main = do
  setStdGen $ mkStdGen 0
  let playGame = recordGame saveDir newGame cpuPlayer
  --putStrLn "Playing games"
  --replicateM 500 playGame
  putStrLn "Training"
  train saveDir

