{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.IORef
import Debug.Trace

import Data.Dequeue
import qualified Data.Dequeue as Dequeue

import Statistics.Sample

import Data.Functor
import Data.Foldable
import Control.Monad
import System.Random
import Control.Monad.State hiding (state)
--import qualified Data.Map as Map

import Prelude hiding (sum)

import AI.HNN.FF.Network
import qualified Data.Packed.Vector as Packed
import qualified Data.Vector.Generic as Vector

import Discrete
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
categoricalInput = (threesToCategorical, tileSizeCategorical)

--(featureFunction, tileSizeInput) = unaryInput
--(featureFunction, tileSizeInput) = binaryInput
(featureFunction, tileSizeInput) = categoricalInput

unaryOutput = (scoreThreesUnary, gridScoreUnary)
binaryOutput = (scoreThreesBinary, gridScoreBinary)
continuousOutput = (scoreThreesContinuous, 1)

--(scoreFunction, outputLayer) = unaryOutput
--(scoreFunction, outputLayer) = binaryOutput
(scoreFunction, outputLayer) = continuousOutput

nnConfig = NNConfig
  { inputs = gridSize * tileSizeInput
  , outputs = outputLayer
  , activation = tanh
  , activation' = tanh'
  , layers = [32]
  , learningRate = 0.0001
  , inputDatum = convert . featureFunction
  , outputDatum = convert . scoreFunction
  , interpret = const . sum . Packed.toList
  , depth = 1
  , batch = 1
  , queue = 500
--  , callback = print . scoreGrid . grid 
  }

{-
playGames = do
  putStrLn "Playing games"
  (replicateM 100 $ recordGame "saved/threes/" (return newGame) cpuPlayer) >>= printResults
  (replicateM 100 $ recordGame "saved/threes/" randomGame cpuPlayer) >>= printResults
  --rand <- replicateM 100 $ recordGame "saved/threes/" (return newGame) randomPlayer
-}

scoreGameState = scoreGrid . grid . state

scorePlayer :: (Fractional w, MonadDiscrete w m) => Player ThreesState m -> m Int
scorePlayer player = do
  (_, final) <- playOutM' player newGame
  return $ scoreGameState final

runPlayer = print . scorePlayer

estimatePlayer :: (Fractional w, MonadDiscrete w m) => Int -> Player ThreesState m -> m [Double]
estimatePlayer samples player = do
  scores <- Packed.fromList <$> replicateM samples (fromIntegral <$> scorePlayer player)
  return $ map ($ scores) [mean, stdDev, Vector.minimum, Vector.maximum]

--type Learner m = m (Player s m)

estimateLearner :: (Foldable q, Dequeue q, MonadIO m) => Int -> m (Player ThreesState IO) -> StateT (q Int) m ()
estimateLearner size learner = do
  player <- lift learner
  score <- liftIO $ scorePlayer player
  --liftIO $ print score
  
  queue <- get
  let queue' = pushBack queue score
  let queue'' = if Dequeue.length queue' > size
                  then snd $ popFront queue'
                  else queue'
  put queue''
  
  let scores = Packed.fromList $ map fromIntegral $ toList queue''
  liftIO $ print $ map ($ scores) [mean, stdDev, Vector.minimum, Vector.maximum]

emptyQueue :: BankersDequeue a
emptyQueue = empty

main = do
  setStdGen $ mkStdGen 0
  modelRef <- initNN nnConfig >>= newIORef
  
  let batchLearner = batchLearn nnConfig modelRef newGame
  let queueLearner = queueLearn nnConfig modelRef newGame
  
  let runBatch = runStateT (forever $ estimateLearner 1000 batchLearner) emptyQueue
  let runQueue = runStateT (runStateT (forever $ estimateLearner 1000 queueLearner) emptyQueue) emptyQueue
  --runStateT (for_ [1..10000] $ \i -> learn >>= (estimatePlayer 1) >>= (liftIO . print)) emptyQueue
  
  runBatch
  --runQueue

  {-
  learned <- readIORef modelRef
  let heuristic = getHeuristic nnConfig learned
  
  forM [1, 4, 7]
    (\depth -> estimatePlayer (lookAheadPlayDepth depth heuristic) 100)
  -}
	  
  --estimatePlayer (cpuPlayer 4) 10
  
  --runPlayer humanPlayer
  
  --estimatePlayer randomPlayer 1000
  --forM [1,4] (\n -> estimatePlayer (cpuPlayer n) 1000)
    
  --forever $ runPlayer cpuPlayer
  
  --recordGame saveDir randomGame cpuPlayer
  
  --playGames
  
  --(trainAndRun "saved/threes" trainer 3 randomGame 10) >>= printResults

