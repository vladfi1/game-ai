{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.IORef
import Debug.Trace

import Data.Functor
import Control.Monad
import Control.Monad.Random
import qualified Data.Map as Map

import AI.HNN.FF.Network
import Data.Packed.Vector

import Discrete (choose)
import Threes
import OnePlayer
import NewGame
import Runner
import TrainNN
import Convertible

humanPlayer game @ PlayerState {actions} = do
  print game
  line <- getLine
  let tile = read line :: Direction
  return $ (Map.fromList actions) Map.! tile

cpuPlayer = (lookAheadPlayDepth 3 basicHeuristic)

saveDir = "saved/threes/"

unaryInput = (threesToUnary, tileSizeUnary)
binaryInput = (threesToBinary, tileSizeBinary)

(featureFunction, tileSizeInput) = unaryInput
--(featureFunction, tileSizeInput) = binaryInput

unaryOutput = (scoreThreesUnary, gridScoreUnary)
binaryOutput = (scoreThreesBinary, gridScoreBinary)
continuousOutput = (scoreThreesContinuous, 1)

(scoreFunction, outputLayer) = unaryOutput
--(scoreFunction, outputLayer) = binaryOutput
--(scoreFunction, outputLayer) = continuousOutput

nnConfig = NNConfig
  { inputs = gridSize * tileSizeInput
  , outputs = outputLayer
  , activation = tanh
  , activation' = tanh'
  , layers = [32]
  , learningRate = 0.1
  , inputDatum = convert . featureFunction
  , outputDatum = convert . scoreFunction
  , interpret = const . sum . toList
  , depth = 3
  , callback = print . scoreGrid . grid 
  }

{-
playGames = do
  putStrLn "Playing games"
  (replicateM 100 $ recordGame "saved/threes/" (return newGame) cpuPlayer) >>= printResults
  (replicateM 100 $ recordGame "saved/threes/" randomGame cpuPlayer) >>= printResults
  --rand <- replicateM 100 $ recordGame "saved/threes/" (return newGame) randomPlayer
-}

main = do
  setStdGen $ mkStdGen 0
  modelRef <- initNN nnConfig >>= newIORef
  
  let learn = (nextPlayer newGame) >>= (tdLearn nnConfig modelRef)
  
  replicateM 10000 learn
  
  --recordGame saveDir randomGame cpuPlayer
  
  --playGames
  
  --(trainAndRun "saved/threes" trainer 3 randomGame 10) >>= printResults

