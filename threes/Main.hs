{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Functor
import Control.Monad
import Control.Monad.Random
import qualified Data.Map as Map

import AI.Training
import AI.Calculation

import Discrete (choose)
import Threes
import NewGame
import Runner
import TrainNN

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

unaryInput = (threesToUnary, tileSizeUnary)
binaryInput = (threesToBinary, tileSizeBinary)

--(featureFunction, tileSizeInput) = unaryInput
(featureFunction, tileSizeInput) = binaryInput

unaryOutput = (scoreThreesUnary, tileSizeUnary)
binaryOutput = (scoreThreesBinary, tileSizeBinary)

(scoreFunction, outputLayer) = unaryOutput
--(scoreFunction, outputLayer) = binaryOutput

project (f, v) = (featureFunction f, scoreFunction v)


modelConfig = ModelConfig
  { activation = Sigmoid
  , costModel = Logistic
  , layers = [gridSize * tileSizeInput, 32, 8, outputLayer]
  , regularization = 0.01
  }

trainConfig = TrainConfig
  { algorithm = LBFGS
  , precision = 0.001
  , iterations = 100
  }

train dir = do
  dataset <- loadData dir symmetrize
  let projected = map project dataset
  
  putStrLn "Subsampling data"
  training <- choose 1000 projected
  validation <- choose 1000 projected
  
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
  --recordGame saveDir newGame cpuPlayer
  --putStrLn "Playing games"
  --replicateM 500 playGame
  train saveDir

