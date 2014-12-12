{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.IORef
import Debug.Trace

import Statistics.Sample

import Data.Functor
import Control.Monad
import System.Random
--import qualified Data.Map as Map

import AI.HNN.FF.Network
import qualified Data.Packed.Vector as Packed
import qualified Data.Vector.Generic as Vector

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
  return $ read line
  --let tile = read line :: Direction
  --return $ (Map.fromList actions) Map.! tile

cpuPlayer n = (lookAheadPlayDepth n emptyHeuristic)

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
  , activation = sigmoid
  , activation' = sigmoid'
  , layers = [32]
  , learningRate = 0.001
  , inputDatum = convert . featureFunction
  , outputDatum = convert . scoreFunction
  , interpret = const . sum . Packed.toList
  , depth = 1
  , callback = print . scoreGrid . grid 
  }

{-
playGames = do
  putStrLn "Playing games"
  (replicateM 100 $ recordGame "saved/threes/" (return newGame) cpuPlayer) >>= printResults
  (replicateM 100 $ recordGame "saved/threes/" randomGame cpuPlayer) >>= printResults
  --rand <- replicateM 100 $ recordGame "saved/threes/" (return newGame) randomPlayer
-}

scoreGameState = scoreGrid . grid . state

runPlayer player = do
  (_, final) <- playOutM' player newGame
  print $ scoreGameState final

estimatePlayer player samples = do
  games <- replicateM samples $ playOutM' player newGame
  let scores = Packed.fromList $ map (fromIntegral . scoreGameState . snd) games
  print $ map ($ scores) [mean, stdDev, Vector.minimum, Vector.maximum]

main = do
  setStdGen $ mkStdGen 0
  modelRef <- initNN nnConfig >>= newIORef
  
  {-
  --let learn = (nextPlayer newGame) >>= (tdLearn nnConfig modelRef)
  let learn = (nextPlayer newGame) >>= (batchLearn nnConfig modelRef 5)
  replicateM 1000 $ (learn >>= print . scoreGrid . grid)
  
  learned <- readIORef modelRef
  let heuristic = getHeuristic nnConfig learned
  
  forM [1, 4, 7]
    (\depth -> estimatePlayer (lookAheadPlayDepth depth heuristic) 100)
  -}
  
  --estimatePlayer (cpuPlayer 4) 10
  
  runPlayer humanPlayer
  
  --estimatePlayer randomPlayer 1000
  --forM [1,4] (\n -> estimatePlayer (cpuPlayer n) 1000)
    
  --forever $ runPlayer cpuPlayer
  
  --recordGame saveDir randomGame cpuPlayer
  
  --playGames
  
  --(trainAndRun "saved/threes" trainer 3 randomGame 10) >>= printResults

