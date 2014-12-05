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

import AI.Training
import AI.Calculation
import AI.Model
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

unaryOutput = (scoreThreesUnary, tileSizeUnary)
binaryOutput = (scoreThreesBinary, gridScoreBinary)

--(scoreFunction, outputLayer) = unaryOutput
(scoreFunction, outputLayer) = binaryOutput

modelConfig = ModelConfig
  { activation = Sigmoid
  , costModel = Logistic
  , layers = [gridSize * tileSizeInput, 32, 16, outputLayer]
  , regularization = 0.01
  }

trainConfig = TrainConfig
  { algorithm = LBFGS
  , precision = 0.001
  , iterations = 100
  }

trainer :: Trainer ThreesState [Bool] [Bool] GenericModel
trainer = Trainer
  { symmetries = return
  , projectIn = featureFunction
  , projectOut = scoreFunction
  , train = doTrain
  , apply = getHeuristic'
  , name = "NN"
  }

debug a = traceShow a a

getHeuristic' :: GenericModel -> [Bool] -> OnePlayer -> Double
getHeuristic' model input _ = rebuild $ getOutput model (convert input)
  where rebuild = sum . toList


doTrain dataset = do
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
  
  return trained

average list = sum list / (fromIntegral $ length list)

printResults results = print $ average (map (fromIntegral . scoreGrid . grid) results)

tdConfig = TDConfig
  { inputDatum = convert . featureFunction
  , outputDatum = convert . scoreFunction
  , interpret = const . sum . toList
  , depth = 3
  , callback = print . scoreGrid . grid 
  }

playGames = do
  putStrLn "Playing games"
  (replicateM 100 $ recordGame "saved/threes/" (return newGame) cpuPlayer) >>= printResults
  (replicateM 100 $ recordGame "saved/threes/" randomGame cpuPlayer) >>= printResults
  --rand <- replicateM 100 $ recordGame "saved/threes/" (return newGame) randomPlayer

main = do
  setStdGen $ mkStdGen 0
  modelRef <- initNN modelConfig >>= newIORef
  
  let learn = (nextPlayer newGame) >>= (tdLearn tdConfig modelRef)
  
  replicateM 1000 learn
  
  
  --recordGame saveDir randomGame cpuPlayer
  
  --playGames
  
  --(trainAndRun "saved/threes" trainer 3 randomGame 10) >>= printResults

